#[macro_use]
extern crate seed;

use futures::future::Future;
use {
    emu6502::{BasicMapper, Cpu, Mapper},
    js_sys::Promise,
    log::debug,
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

struct RomSelectionModel {
    message: Option<String>,
}

impl Default for RomSelectionModel {
    fn default() -> Self {
        RomSelectionModel { message: None }
    }
}

struct RomLoadedModel<M: Mapper> {
    _cpu: Cpu<M>,
}

enum Model<M: Mapper> {
    RomSelection(RomSelectionModel),
    RomLoaded(RomLoadedModel<M>),
}

impl<M: Mapper> Model<M> {
    fn transition_to_rom_selection(&mut self, message: Option<String>) {
        *self = Model::RomSelection(RomSelectionModel { message })
    }

    fn transition_to_rom_loaded(&mut self, cpu: Cpu<M>) {
        *self = Model::RomLoaded(RomLoadedModel { _cpu: cpu })
    }
}

impl<M: Mapper> Default for Model<M> {
    fn default() -> Self {
        Model::RomSelection(RomSelectionModel::default())
    }
}

#[derive(Clone)]
enum Msg {
    RomSelected(Event),
    RomBytesRead(js_sys::Uint8Array),
    RomLoadingErr,
}

fn update<M: Mapper + 'static>(msg: Msg, model: &mut Model<M>, orders: &mut Orders<Msg>) {
    match msg {
        Msg::RomSelected(event) => {
            let file_input: HtmlInputElement = event.target().unwrap().dyn_into().unwrap();
            if let Some(file) = file_input.files().unwrap().get(0) {
                debug!("file selected!");
                //                let file_reader = FileReaderSync::new().unwrap();
                //                let buffer = file_reader.read_as_array_buffer(&file).unwrap();
                //                let bytes = js_sys::Uint8Array::new(&buffer);
                let promise = read_as_array_buffer(file);
                let future = JsFuture::from(promise)
                    .map(|arr| Msg::RomBytesRead(arr.dyn_into::<js_sys::Uint8Array>().unwrap()))
                    .map_err(|_| Msg::RomLoadingErr);

                orders.perform_cmd(future);
            } else {
                debug!("No file selected!");
                model.transition_to_rom_selection(Some("No file was selected".to_owned()));
            }
        }
        Msg::RomBytesRead(bytes) => {
            let mut rom = vec![0; bytes.length() as usize];
            bytes.copy_to(&mut rom);
            let mut cursor = Cursor::new(&rom);
            let mapper = M::new(&mut cursor, 0xa);
            let cpu = Cpu::new(mapper);
            model.transition_to_rom_loaded(cpu);
        }
        Msg::RomLoadingErr => debug!("Rom loading error!"),
    }
}

fn view<M: Mapper>(model: &Model<M>) -> El<Msg> {
    match model {
        Model::RomSelection(model) => div![
            error_message(&model),
            label![
                "ROM Location",
                input![
                    attrs! { At::Type => "file"
                    },
                    raw_ev(Ev::Change, Msg::RomSelected)
                ]
            ]
        ],

        Model::RomLoaded(_rom_loaded_model) => div!["Rom loaded!"],
    }
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
