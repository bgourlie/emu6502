#[macro_use]
extern crate seed;

use disasm6502::Instruction;
use futures::future::Future;
use {
    disasm6502::Disassembly,
    emu6502::{BasicMapper, Cpu, Mapper},
    js_sys::Promise,
    log::debug,
    seed::prelude::*,
    std::{borrow::Cow, io::Cursor},
    wasm_bindgen::JsCast,
    wasm_bindgen_futures::JsFuture,
    web_sys::{Event, File, HtmlInputElement},
};

#[wasm_bindgen(module = "/util.js")]
extern "C" {
    fn read_as_array_buffer(path: File) -> Promise;
}

type Str = Cow<'static, str>;

struct RomSelectionModel {
    message: Option<Str>,
}

impl Default for RomSelectionModel {
    fn default() -> Self {
        RomSelectionModel { message: None }
    }
}

struct RomLoadedModel<M: Mapper> {
    cpu: Cpu<M>,
    disassembly: Disassembly,
}

enum Model<M: Mapper> {
    RomSelection(RomSelectionModel),
    RomLoaded(RomLoadedModel<M>),
}

impl<M: Mapper> Model<M> {
    fn transition_to_rom_selection(&mut self, message: Option<Str>) {
        *self = Model::RomSelection(RomSelectionModel { message })
    }

    fn transition_to_rom_loaded(&mut self, cpu: Cpu<M>, disassembly: Disassembly) {
        *self = Model::RomLoaded(RomLoadedModel { cpu, disassembly })
    }
}

impl<M: Mapper> Default for Model<M> {
    fn default() -> Self {
        Model::RomSelection(RomSelectionModel::default())
    }
}

#[derive(Copy, Clone)]
enum RunStrategy {
    Steps(u64),
    _UntilNmi,
    _Indefinitely,
}

#[derive(Clone)]
enum Msg {
    RomSelected(Event),
    RomBytesRead(js_sys::Uint8Array),
    RomLoadingErr,
    Run(RunStrategy),
}

fn update<M: Mapper + 'static>(msg: Msg, model: &mut Model<M>, orders: &mut Orders<Msg>) {
    match msg {
        Msg::RomSelected(event) => {
            let file_input: HtmlInputElement = event.target().unwrap().dyn_into().unwrap();
            if let Some(file) = file_input.files().unwrap().get(0) {
                debug!("file selected!");
                let promise = read_as_array_buffer(file);
                let future = JsFuture::from(promise)
                    .map(|arr| Msg::RomBytesRead(arr.dyn_into::<js_sys::Uint8Array>().unwrap()))
                    .map_err(|_| Msg::RomLoadingErr);

                orders.perform_cmd(future);
            } else {
                debug!("No file selected!");
                model.transition_to_rom_selection(Some("No file was selected".into()));
            }
        }
        Msg::RomBytesRead(bytes) => {
            let mut rom = vec![0; bytes.length() as usize];
            bytes.copy_to(&mut rom);
            let mut cursor = Cursor::new(&rom);
            let mapper = M::new(&mut cursor, 0xa);
            let disassembly = Disassembly::from_rom(&mut cursor, 0xa, 0x400).unwrap();
            let mut cpu = Cpu::new(mapper);
            cpu.set_pc(0x400); // this is specific to 6502_functional_test.bin
            model.transition_to_rom_loaded(cpu, disassembly);
        }
        Msg::RomLoadingErr => debug!("Rom loading error!"),
        Msg::Run(strategy) => match strategy {
            RunStrategy::Steps(steps) => {
                if let Model::RomLoaded(ref mut model) = model {
                    for _i in 0..steps {
                        model.cpu.step();
                        debug!("stepped cpu pc = {:4X}", model.cpu.pc());
                    }
                } else {
                    panic!("this should never happen")
                }
            }
            _ => unimplemented!("RunStrategy not implemented"),
        },
    }
}

fn view<M: Mapper>(model: &Model<M>) -> El<Msg> {
    match model {
        Model::RomSelection(model) => div![
            attrs! {At::Id => "romSelectionView"},
            error_message(&model),
            label![
                icon("folder"),
                div!["Select ROM"],
                input![
                    attrs! { At::Type => "file"
                    },
                    raw_ev(Ev::Change, Msg::RomSelected)
                ]
            ]
        ],

        Model::RomLoaded(model) => div![
            attrs! {At::Id => "romLoadedView"},
            button![
                "Step",
                simple_ev(Ev::Click, Msg::Run(RunStrategy::Steps(1)))
            ],
            status_widget(&model.cpu),
            disassembly(&model.disassembly, 0x400)
        ],
    }
}

fn icon(name: &'static str) -> El<Msg> {
    div![attrs! {At::Class => format!("icon icon-{}", name)}]
}

fn status_widget<M: Mapper>(cpu: &Cpu<M>) -> El<Msg> {
    div![
        attrs! {At::Id => "cpuStatus"},
        div![div!["pc"], div![format!("{:04X}", cpu.pc())]],
        div![div!["sp"], div![format!("{:02X}", cpu.sp())]],
        div![div!["acc"], div![format!("{:02X}", cpu.acc())]],
        div![div!["x"], div![format!("{:02X}", cpu.x())]],
        div![div!["y"], div![format!("{:02X}", cpu.y())]],
        div![div!["NVssDIZC"], div![format!("{:08b}", cpu.status())]]
    ]
}

fn disassembly(disassembly: &Disassembly, offset: u16) -> El<Msg> {
    let disassembly_rows: Vec<El<Msg>> = disassembly
        .range(offset.saturating_sub(100)..offset.saturating_add(100))
        .map(|(addr, i)| {
            div![
                div![attrs! {At::Class => "addr"}, format!("{:04X}", addr)],
                div![format!("{:?} {}", i.opcode(), i.operand().to_string())]
            ]
        })
        .collect();

    div![attrs! {At::Id => "disassembly"}, disassembly_rows]
}

fn error_message(model: &RomSelectionModel) -> El<Msg> {
    if let Some(ref message) = model.message {
        span![message]
    } else {
        empty![]
    }
}

#[wasm_bindgen(start)]
pub fn bootstrap() {
    wasm_logger::init(wasm_logger::Config::new(log::Level::Debug).message_on_new_line());

    seed::App::build(Model::<BasicMapper>::default(), update, view)
        .finish()
        .run();
}
