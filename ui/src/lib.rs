#[macro_use]
extern crate seed;

use {
    emu6502::{BasicMapper, Cpu},
    log::debug,
    seed::prelude::*,
    std::io::Cursor,
    wasm_bindgen::JsCast,
    web_sys::{Event, FileReader, HtmlInputElement},
};

struct RomSelectionModel {
    message: Option<String>,
}

impl Default for RomSelectionModel {
    fn default() -> Self {
        RomSelectionModel { message: None }
    }
}

enum Model {
    RomSelection(RomSelectionModel),
    RomLoaded,
}

impl Model {
    fn transition_to_rom_selection(&mut self, message: Option<String>) {
        *self = Model::RomSelection(RomSelectionModel { message })
    }

    fn transition_to_rom_loaded(&mut self) {
        *self = Model::RomLoaded
    }
}

impl Default for Model {
    fn default() -> Self {
        Model::RomSelection(RomSelectionModel::default())
    }
}

#[derive(Clone)]
enum Msg {
    RomSelected(Event),
}

fn update(msg: Msg, model: &mut Model, _: &mut Orders<Msg>) {
    match msg {
        Msg::RomSelected(event) => {
            let file_input: HtmlInputElement = event.target().unwrap().dyn_into().unwrap();
            if let Some(file) = file_input.files().unwrap().get(0) {
                debug!("file selected!");

                let file_reader = FileReader::new().unwrap();
                let mut onload = Closure::wrap(Box::new(move |event: Event| {
                    let file_reader: FileReader = event.target().unwrap().dyn_into().unwrap();
                    let mut rom = {
                        let blob = file_reader.result().unwrap();
                        let bytes = js_sys::Uint8Array::new(&blob);
                        let mut rom = vec![0; bytes.length() as usize];
                        bytes.copy_to(&mut rom);
                        Cursor::new(rom)
                    };
                    let mapper = BasicMapper::new(&mut rom, 0x0a);
                    let cpu = Cpu::new(mapper);
                    debug!("cpu loaded!");
                }) as Box<dyn FnMut(_)>);

                file_reader.set_onload(Some(onload.as_ref().unchecked_ref()));
                file_reader.read_as_array_buffer(&file).unwrap();
                onload.forget();

                model.transition_to_rom_loaded();
            } else {
                debug!("No file selected!");
                model.transition_to_rom_selection(Some("No file was selected".to_owned()));
            }
        }
    }
}

fn view(model: &Model) -> El<Msg> {
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

        Model::RomLoaded => div!["Rom loaded!"],
    }
}

fn error_message(model: &RomSelectionModel) -> El<Msg> {
    if let Some(ref message) = model.message {
        span![message]
    } else {
        empty![]
    }
}

#[wasm_bindgen]
pub fn bootstrap() {
    wasm_logger::init(wasm_logger::Config::new(log::Level::Debug).message_on_new_line());

    seed::App::build(Model::default(), update, view)
        .finish()
        .run();
}
