// use wasm_bindgen::prelude::wasm_bindgen;


// #[wasm_bindgen(raw_module = "vscode")]
// extern "C" {
//     pub type LogOutputChannel;
    
//     #[wasm_bindgen(method)]
//     fn info(this: &LogOutputChannel, message: &str);
//     #[wasm_bindgen(method)]
//     fn debug(this: &LogOutputChannel, message: &str);
// }


// pub mod logger {
//     use std::cell::RefCell;
//     use super::LogOutputChannel;

//     thread_local!(static LOGGER: RefCell<Option<LogOutputChannel>> = RefCell::new(None));
//     pub fn set_logger(logger: LogOutputChannel) {
//         LOGGER.with(|l| {
//             *l.borrow_mut() = Some(logger);
//         });
//     }

//     pub fn info(message: &str) {
//         LOGGER.with(|logger| {
//             logger.borrow().as_ref().unwrap().info(message);
//         });
//     }
//     pub fn debug(message: &str) {
//         LOGGER.with(|logger| {
//             logger.borrow().as_ref().unwrap().debug(message);
//         });
//     }
// }