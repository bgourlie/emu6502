#[macro_use]
extern crate seed;

mod console_buffer;

use {
    console_buffer::ConsoleBuffer,
    disasm6502::Disassembly,
    emu6502::{BasicMapper, Cpu, DebuggableCpu, Debugger, Mapper},
    futures::future::Future,
    js_sys::Promise,
    log::{debug, warn},
    seed::prelude::*,
    std::io::Cursor,
    wasm_bindgen::JsCast,
    wasm_bindgen_futures::JsFuture,
    web_sys::{Event, File, HtmlInputElement},
};

#[wasm_bindgen(module = "/util.js")]
extern "C" {
    fn read_as_array_buffer(path: File) -> Promise;
}

struct RomLoadedModel<M: Mapper + Debugger> {
    cpu: Cpu<M>,
    disassembly: Disassembly,
}

struct Model<M: Mapper + Debugger> {
    console_buffer: ConsoleBuffer,
    state: State<M>,
}

impl<M: Mapper + Debugger> Model<M> {
    fn transition_to_rom_loaded(&mut self, cpu: Cpu<M>, disassembly: Disassembly) {
        self.state = State::RomLoaded(RomLoadedModel { cpu, disassembly })
    }
}

enum State<M: Mapper + Debugger> {
    RomSelection,
    RomLoaded(RomLoadedModel<M>),
}

impl<M: Mapper + Debugger> Default for Model<M> {
    fn default() -> Self {
        let mut console_buffer = ConsoleBuffer::default();
        console_buffer.push("Welcome!");
        Model {
            state: State::RomSelection,
            console_buffer,
        }
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
    KeyPress(web_sys::KeyboardEvent),
}

fn update<M: Mapper + Debugger + 'static>(
    msg: Msg,
    model: &mut Model<M>,
    orders: &mut Orders<Msg>,
) {
    match msg {
        Msg::KeyPress(event) => match event.key_code() {
            83 => {
                if let State::RomLoaded(_) = model.state {
                    orders.send_msg(Msg::Run(RunStrategy::Steps(1)));
                }
            }
            _ => (),
        },
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
                model.console_buffer.push("No file was selected");
            }
        }
        Msg::RomBytesRead(bytes) => {
            let mut rom = vec![0; bytes.length() as usize];
            bytes.copy_to(&mut rom);
            let mut cursor = Cursor::new(&rom);
            let mapper = M::new(&mut cursor, 0xa);
            let disassembly = Disassembly::from_stream(&mut cursor, 0xa, Some(0x400)).unwrap();
            let mut cpu = Cpu::new(mapper);
            cpu.set_pc(0x400); // this is specific to 6502_functional_test.bin
            model.transition_to_rom_loaded(cpu, disassembly);
        }
        Msg::RomLoadingErr => debug!("Rom loading error!"),
        Msg::Run(strategy) => match strategy {
            RunStrategy::Steps(steps) => {
                if let State::RomLoaded(ref mut state) = model.state {
                    for _i in 0..steps {
                        state.cpu.step();
                        debug!("stepped cpu pc = {:4X}", state.cpu.pc());
                    }

                    let memory_changes = state.cpu.mapper_mut().read_memory_changes();
                    if !memory_changes.is_empty() {
                        // Retrieve any updated offsets that are disassembled program code
                        let mut offsets_to_disassemble =
                            state.disassembly.instruction_offsets(memory_changes);

                        // If the program counter points to an an offset that has not been
                        // disassembled, ensure it is also disassembled
                        if !offsets_to_disassemble.contains(&state.cpu.pc()) {
                            offsets_to_disassemble.insert(state.cpu.pc());
                        }

                        if !offsets_to_disassemble.is_empty() {
                            let mut stream = state.cpu.mapper().address_space_stream();
                            state
                                .disassembly
                                .update(&mut stream, offsets_to_disassemble)
                                .expect("disassembly update failed");
                        }
                    }
                } else {
                    panic!("this should never happen")
                }
            }
            _ => unimplemented!("RunStrategy not implemented"),
        },
    }
}

fn view<M: Mapper + Debugger>(model: &Model<M>) -> El<Msg> {
    let (top_view, main_view) = match model.state {
        State::RomSelection => {
            let top_view = div![label![
                id!["romSelectButton"],
                icon("folder"),
                div!["Select ROM"],
                input![
                    attrs! { At::Type => "file"
                    },
                    raw_ev(Ev::Change, Msg::RomSelected)
                ]
            ]];
            let main_view = div![];
            (top_view, main_view)
        }

        State::RomLoaded(ref state) => {
            let top_view = div![
                status_widget(&state.cpu),
                button![
                    "Step",
                    simple_ev(Ev::Click, Msg::Run(RunStrategy::Steps(1)))
                ],
            ];

            let main_view = div![
                id!["romLoadedView"],
                disassembly(&state.disassembly, state.cpu.pc())
            ];
            (top_view, main_view)
        }
    };

    let console_rows: Vec<El<Msg>> = model.console_buffer.iter().map(|s| div![s]).collect();
    debug!("{}", console_rows.len());
    let top_view = top_view.add_attr("id".to_owned(), "topRow".to_owned());
    let main_view = main_view
        .add_attr("id".to_owned(), "mainRow".to_owned())
        .add_child(div![id!["console"], console_rows]);

    div![
        id!["view"],
        keyboard_ev("keydown", Msg::KeyPress),
        top_view,
        main_view,
    ]
}

fn icon(name: &'static str) -> El<Msg> {
    div![attrs! {At::Class => format!("icon icon-{}", name)}]
}

fn status_widget<M: Mapper + Debugger>(cpu: &Cpu<M>) -> El<Msg> {
    div![
        id!["cpuStatus"],
        div![div!["pc"], div![format!("{:04X}", cpu.pc())]],
        div![div!["sp"], div![format!("{:02X}", cpu.sp())]],
        div![div!["acc"], div![format!("{:02X}", cpu.acc())]],
        div![div!["x"], div![format!("{:02X}", cpu.x())]],
        div![div!["y"], div![format!("{:02X}", cpu.y())]],
        div![div!["NVssDIZC"], div![format!("{:08b}", cpu.status())]]
    ]
}

fn disassembly(disassembly: &Disassembly, offset: u16) -> El<Msg> {
    if disassembly.instruction_at(offset) == None {
        warn!("No instruction at {:04X}", offset);
    }

    let disassembly_rows: Vec<El<Msg>> = disassembly
        .window(offset, 100)
        .map(|(addr, i)| {
            let instruction_classes = if *addr != offset {
                class!["instruction"]
            } else {
                class!["instruction", "current"]
            };

            div![
                div![class!["gutter"], div![format!("{:04X}", addr)]],
                div![
                    instruction_classes,
                    format!("{:?} {}", i.opcode(), i.operand().to_string())
                ]
            ]
        })
        .collect();

    div![id!["disassembly"], disassembly_rows]
}

#[wasm_bindgen(start)]
pub fn bootstrap() {
    wasm_logger::init(wasm_logger::Config::new(log::Level::Debug).message_on_new_line());

    seed::App::build(Model::<BasicMapper>::default(), update, view)
        .finish()
        .run();
}
