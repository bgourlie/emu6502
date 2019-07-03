#![feature(custom_inner_attributes)]
#![rustfmt::skip::macros(div, input, label)]

#[macro_use]
extern crate seed;

use {log::debug, seed::prelude::*};

// Model

struct Model {
    rom_path: Option<String>,
}

impl Model {
    fn rom_path_display(&self) -> String {
        if let Some(rom_path) = &self.rom_path {
            rom_path.to_string()
        } else {
            "None Selected".to_owned()
        }
    }
}

impl Default for Model {
    fn default() -> Self {
        Self { rom_path: None }
    }
}

// Update

#[derive(Clone)]
enum Msg {
    RomSelected(String),
}

fn update(msg: Msg, model: &mut Model, _: &mut Orders<Msg>) {
    match msg {
        Msg::RomSelected(path) => {
            if path.trim().is_empty() {
                debug!("No file selected.");
                model.rom_path = None;
            } else {
                debug!("Selected {}", path);
                model.rom_path = Some(path);
            }
        }
    }
}

// View

fn view(model: &Model) -> El<Msg> {
    div!
    [ label!
        [ "ROM Location"
        , input!
            [ attrs!
                { At::Type => "file"
                }
            , input_ev(Ev::Change, Msg::RomSelected)
            ]
        ]
        , span![ model.rom_path_display() ]
    ]
}

#[wasm_bindgen]
pub fn bootstrap() {
    wasm_logger::init(wasm_logger::Config::new(log::Level::Debug).message_on_new_line());

    seed::App::build(Model::default(), update, view)
        .finish()
        .run();
}
