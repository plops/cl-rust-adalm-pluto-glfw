#[allow(unused_parens)]
use chrono::{DateTime, Utc};
extern crate chrono;
extern crate core_affinity;
extern crate crossbeam_channel;
extern crate fftw;
extern crate glfw;
extern crate imgui;
extern crate imgui_glfw_rs;
extern crate industrial_io as iio;
mod iio_reader;
use glfw::{Action, Context, Key};
use std::ffi::CString;
use std::os::raw::c_void;
fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();
    glfw.window_hint(glfw::WindowHint::ContextVersion(3, 3));
    let (mut window, events) = glfw
        .create_window(512, 512, "glfw win", glfw::WindowMode::Windowed)
        .expect("failed to create glfw window");
    window.make_current();
    window.set_all_polling(true);
    gl::load_with(|symbol| {
        return window.get_proc_address(symbol);
    });
    unsafe {
        gl::Enable(gl::BLEND);
        gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);
        gl::Enable(gl::DEPTH_TEST);
        gl::DepthFunc(gl::LESS);
        gl::ClearColor(0.10, 0.10, 0.10, 1.0);
        let mut texture = 0;
        gl::GenTextures(1, &mut texture);
        gl::BindTexture(gl::TEXTURE_2D, texture);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, (gl::REPEAT as i32));
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, (gl::REPEAT as i32));
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, (gl::LINEAR as i32));
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, (gl::LINEAR as i32));
        gl::TexImage2D(
            gl::TEXTURE_2D,
            0,
            (gl::RGB as i32),
            (4096 as i32),
            (128 as i32),
            0,
            gl::RGB,
            gl::UNSIGNED_BYTE,
            ((&(data[0]) as *const u8) as *const c_void),
        );
    }
    let mut imgui = imgui::Context::create();
    let mut imgui_glfw = imgui_glfw_rs::ImguiGLFW::new(&mut imgui, &mut window);
    imgui.set_ini_filename(None);
    while (!(window.should_close())) {
        unsafe {
            gl::Clear(((gl::COLOR_BUFFER_BIT) | (gl::DEPTH_BUFFER_BIT)));
        }
        {
            let ui = imgui_glfw.frame(&mut window, &mut imgui);
            ui.show_metrics_window(&mut true);
            ui.text("bla");
            ui.show_demo_window(&mut true);
            imgui_glfw.draw(ui, &mut window);
        }
        window.swap_buffers();
        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            println!("{:?}", event);
            imgui_glfw.handle_event(&mut imgui, &event);
            match event {
                glfw::WindowEvent::Key(Key::Escape, _, Action::Press, _) => {
                    window.set_should_close(true)
                }
                _ => {}
            }
        }
    }
}
