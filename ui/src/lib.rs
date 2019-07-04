#[macro_use]
extern crate seed;

use {
    log::debug,
    seed::prelude::*,
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
            if let Some(_file) = file_input.files().unwrap().get(0) {
                debug!("file selected!");
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
