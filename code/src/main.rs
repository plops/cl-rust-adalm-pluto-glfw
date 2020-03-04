#[allow(unused_parens)]
use chrono::{DateTime, Utc};
#[macro_use]
extern crate imgui;
extern crate chrono;
extern crate core_affinity;
extern crate crossbeam_channel;
extern crate fftw;
extern crate imgui_glfw_rs;
extern crate industrial_io as iio;
use crossbeam_channel::bounded;
use fftw::plan::C2CPlan;
use imgui_glfw_rs::glfw::{Action, Context, Key};
use imgui_glfw_rs::ImguiGLFW;
use std::ffi::CString;
use std::io;
use std::os::raw::c_void;
use std::sync::{Arc, Mutex};
use std::thread::spawn;
pub struct SendComplex {
    pub timestamp: DateTime<Utc>,
    pub ptr: fftw::array::AlignedVec<num_complex::Complex<f64>>,
}
unsafe impl Send for SendComplex {}
fn main() {
    let (s0, r0) = crossbeam_channel::bounded(3);
    let (s1, r1) = crossbeam_channel::bounded(3);
    let mut fftin = [
        std::sync::Arc::new(std::sync::Mutex::new(SendComplex {
            timestamp: Utc::now(),
            ptr: fftw::array::AlignedVec::new(512),
        })),
        std::sync::Arc::new(std::sync::Mutex::new(SendComplex {
            timestamp: Utc::now(),
            ptr: fftw::array::AlignedVec::new(512),
        })),
        std::sync::Arc::new(std::sync::Mutex::new(SendComplex {
            timestamp: Utc::now(),
            ptr: fftw::array::AlignedVec::new(512),
        })),
    ];
    let mut fftout = [
        std::sync::Arc::new(std::sync::Mutex::new(SendComplex {
            timestamp: Utc::now(),
            ptr: fftw::array::AlignedVec::new(512),
        })),
        std::sync::Arc::new(std::sync::Mutex::new(SendComplex {
            timestamp: Utc::now(),
            ptr: fftw::array::AlignedVec::new(512),
        })),
        std::sync::Arc::new(std::sync::Mutex::new(SendComplex {
            timestamp: Utc::now(),
            ptr: fftw::array::AlignedVec::new(512),
        })),
    ];
    let core_ids = core_affinity::get_core_ids().unwrap();
    let b = std::thread::Builder::new().name("pluto_reader".into());
    let reader_thread = b.spawn(move || {
        core_affinity::set_for_current(core_affinity::CoreId { id: 0 });
        let ctx = iio::Context::create_network("192.168.2.1").unwrap_or_else(|err_| {
            {
                println!(
                    "{} {}:{} couldnt open iio context ",
                    Utc::now(),
                    file!(),
                    line!()
                );
            }
            std::process::exit(1);
        });
        let mut trigs = Vec::new();
        for dev in ctx.devices() {
            if dev.is_trigger() {
                match dev.id() {
                    Some(id) => trigs.push(id),
                    None => (),
                }
            } else {
                println!(
                    "{} [{}]: {} channels",
                    dev.id().unwrap_or_default(),
                    dev.name().unwrap_or_default(),
                    dev.num_channels()
                );
            }
        }
        if trigs.is_empty() {
            {
                println!("{} {}:{} no triggers ", Utc::now(), file!(), line!());
            }
        } else {
            for s in trigs {
                println!("trigger {}", s);
            }
        };
        let dev = ctx.find_device("cf-ad9361-lpc").unwrap_or_else(|| {
            {
                println!(
                    "{} {}:{} no device named cf-ad9361-lpc ",
                    Utc::now(),
                    file!(),
                    line!()
                );
            }
            std::process::exit(2);
        });
        let phy = ctx.find_device("ad9361-phy").unwrap_or_else(|| {
            {
                println!(
                    "{} {}:{} no device named ad9361-phy ",
                    Utc::now(),
                    file!(),
                    line!()
                );
            }
            std::process::exit(2);
        });
        let mut nchan = 0;
        for mut chan in dev.channels() {
            if (Some(std::any::TypeId::of::<i16>())) == (chan.type_of()) {
                nchan += 1;
                chan.enable();
            };
        }
        if (0) == (nchan) {
            {
                println!(
                    "{} {}:{} no 16 bit channels found ",
                    Utc::now(),
                    file!(),
                    line!()
                );
            }
            std::process::exit(1);
        } else {
            {
                println!(
                    "{} {}:{} 16 bit channels found  nchan={}",
                    Utc::now(),
                    file!(),
                    line!(),
                    nchan
                );
            }
        };
        let mut chans = Vec::new();
        let mut buf = dev.create_buffer(512, false).unwrap_or_else(|err| {
            {
                println!(
                    "{} {}:{} can't create buffer  err={}",
                    Utc::now(),
                    file!(),
                    line!(),
                    err
                );
            }
            std::process::exit(3);
        });
        for ch in dev.channels() {
            chans.push(ch);
        }
        crossbeam_utils::thread::scope(|scope| {
            scope.spawn(|_| {
                {
                    println!("{} {}:{} start fftw plan ", Utc::now(), file!(), line!());
                }
                let mut plan: fftw::plan::C2CPlan64 = fftw::plan::C2CPlan::aligned(
                    &[512],
                    fftw::types::Sign::Forward,
                    fftw::types::Flag::Measure,
                )
                .unwrap();
                {
                    println!("{} {}:{} finish fftw plan ", Utc::now(), file!(), line!());
                }
                loop {
                    let tup: usize = r0.recv().ok().unwrap();
                    let mut ha = fftin[tup].clone();
                    let mut a = &mut ha.lock().unwrap();
                    let mut hb = fftout[tup].clone();
                    let mut b = &mut hb.lock().unwrap();
                    plan.c2c(&mut a.ptr, &mut b.ptr).unwrap();
                    b.timestamp = Utc::now();
                    {
                        println!(
                            "{} {}:{}   tup={}  (b.timestamp-a.timestamp)={}  b.ptr[0]={}",
                            Utc::now(),
                            file!(),
                            line!(),
                            tup,
                            (b.timestamp - a.timestamp),
                            b.ptr[0]
                        );
                    }
                    s1.send(tup).unwrap();
                }
            });
            let mut count = 0;
            loop {
                match buf.refill() {
                    Err(err) => {
                        {
                            println!(
                                "{} {}:{} error filling buffer  err={}",
                                Utc::now(),
                                file!(),
                                line!(),
                                err
                            );
                        }
                        std::process::exit(4)
                    }
                    _ => (),
                }
                {
                    let time_acquisition = Utc::now();
                    let mut ha = fftin[count].clone();
                    let mut a = &mut ha.lock().unwrap();
                    let data_i: Vec<i16> = buf.channel_iter::<i16>(&(chans[0])).collect();
                    let data_q: Vec<i16> = buf.channel_iter::<i16>(&(chans[1])).collect();
                    a.timestamp = time_acquisition;
                    for i in 0..512 {
                        a.ptr[i] = fftw::types::c64::new((data_i[i] as f64), (data_q[i] as f64));
                    }
                }
                {
                    println!(
                        "{} {}:{} sender  count={}",
                        Utc::now(),
                        file!(),
                        line!(),
                        count
                    );
                }
                s0.send(count).unwrap();
                count += 1;
                if (3) <= (count) {
                    count = 0;
                };
            }
        })
        .unwrap();
    });
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
    let mut data = Vec::with_capacity(((128) * (128)));
    let mut texture_id;
    for i in 0..128 {
        for j in 0..128 {
            data.push((j as u8));
            data.push((i as u8));
            data.push(((j + i) as u8));
        }
    }
    unsafe {
        gl::Enable(gl::BLEND);
        gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);
        gl::Enable(gl::DEPTH_TEST);
        gl::DepthFunc(gl::LESS);
        gl::ClearColor(0.10, 0.10, 0.10, 1.0);
        let mut texture = 0;
        gl::GenTextures(1, &mut texture);
        texture_id = imgui::TextureId::from((texture as usize));
        gl::BindTexture(gl::TEXTURE_2D, texture);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, (gl::REPEAT as i32));
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, (gl::REPEAT as i32));
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, (gl::LINEAR as i32));
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, (gl::LINEAR as i32));
        gl::TexImage2D(
            gl::TEXTURE_2D,
            0,
            (gl::RGB as i32),
            (128 as i32),
            (128 as i32),
            0,
            gl::RGB,
            gl::UNSIGNED_BYTE,
            ((&(data[0]) as *const u8) as *const c_void),
        );
    };
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
            imgui::Window::new(&ui, im_str!("hello")).build(|| {
                ui.text("bla2");
                ui.image(texture_id, [128., 128.]).build();
                ui.text("bla3");
            });
            ui.show_demo_window(&mut true);
            imgui_glfw.draw(ui, &mut window);
        }
        window.swap_buffers();
        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
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
