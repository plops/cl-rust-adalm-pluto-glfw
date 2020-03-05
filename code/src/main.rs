#[allow(unused_parens)]
#[allow(unused_imports)]
#[allow(unused_variable)]
#[allow(unused_mut)]
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
use std::sync::{atomic, Arc, Mutex};
use std::thread::spawn;
// for fftw to be fast storage in the data processing pipeline must be aligned for simd (on 16 byte boundary). the fftw package comes with a type for this.
pub struct SendComplex {
    pub timestamp: DateTime<Utc>,
    pub ptr: fftw::array::AlignedVec<num_complex::Complex<f64>>,
}
// the following is required to tell rust that we can send pointers to complex arrays between threads
unsafe impl Send for SendComplex {}
fn main() {
    let mut keep_running = std::sync::atomic::AtomicBool::new(true);
    // dataprocessing pipeline:
    // each stage runs in a thread, they communicated via channels
    // s0,r0 sdr_receiver -> fft_processor
    // s1,r1 are for fft_processor -> fft_scaler
    // s2,r2 fft_scaler -> opengl
    // s0 and s2 are bounded to 3 or 4, the processing seems to be fast enough to ever create back pressure (and loose sdr_receiver chunks)
    // size of bounded s2 channel has to be large enough to store chunks that are acquired while waiting for next vsync
    let (s0, r0) = crossbeam_channel::bounded(4);
    let barrier_pipeline_setup = std::sync::Arc::new(std::sync::Barrier::new(3));
    let (s1, r1) = crossbeam_channel::bounded(4);
    let (s2, r2) = crossbeam_channel::bounded(40);
    // pipeline storage:
    // fftin is filled by sdr_receiver thread and consumed by fft_processor thread
    // fftout is filled by fft_processor and consumed by fft_scaler
    // fftout_scaled is filled by fft_scaler and consumed by the gui thread
    let mut fftin = [
        std::sync::Arc::new(std::sync::Mutex::new(SendComplex {
            timestamp: Utc::now(),
            ptr: fftw::array::AlignedVec::new(256),
        })),
        std::sync::Arc::new(std::sync::Mutex::new(SendComplex {
            timestamp: Utc::now(),
            ptr: fftw::array::AlignedVec::new(256),
        })),
        std::sync::Arc::new(std::sync::Mutex::new(SendComplex {
            timestamp: Utc::now(),
            ptr: fftw::array::AlignedVec::new(256),
        })),
        std::sync::Arc::new(std::sync::Mutex::new(SendComplex {
            timestamp: Utc::now(),
            ptr: fftw::array::AlignedVec::new(256),
        })),
    ];
    let mut fftout = [
        std::sync::Arc::new(std::sync::Mutex::new(SendComplex {
            timestamp: Utc::now(),
            ptr: fftw::array::AlignedVec::new(256),
        })),
        std::sync::Arc::new(std::sync::Mutex::new(SendComplex {
            timestamp: Utc::now(),
            ptr: fftw::array::AlignedVec::new(256),
        })),
        std::sync::Arc::new(std::sync::Mutex::new(SendComplex {
            timestamp: Utc::now(),
            ptr: fftw::array::AlignedVec::new(256),
        })),
        std::sync::Arc::new(std::sync::Mutex::new(SendComplex {
            timestamp: Utc::now(),
            ptr: fftw::array::AlignedVec::new(256),
        })),
    ];
    let mut fftout_scaled: [Arc<Mutex<[f32; 256]>>; 40] = [
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
        std::sync::Arc::new(std::sync::Mutex::new([0.0; 256])),
    ];
    // start all the threads in a crossbeam scope, so that they can access the pipeline storage without Rust making it too difficult
    // before the pipeline starts working all threads wait at a barrier until the fftw thread has been initialized
    // when the gui is exited (by pressing esc key in the window) all threads are notified to quit by the atomic variable keep_running
    crossbeam_utils::thread::scope(|scope| {
        scope.builder().name("gui".into()).spawn(|_| {
            let wg = barrier_pipeline_setup.clone();
            let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();
            glfw.window_hint(glfw::WindowHint::ContextVersion(3, 3));
            {
                println!("{} {}:{} gui starts ", Utc::now(), file!(), line!());
            }
            let (mut window, events) = glfw
                .create_window(512, 512, "glfw win", glfw::WindowMode::Windowed)
                .expect("failed to create glfw window");
            window.make_current();
            window.set_all_polling(true);
            gl::load_with(|symbol| {
                return window.get_proc_address(symbol);
            });
            let mut data = Vec::with_capacity(((256) * (512)));
            let mut texture_id;
            for i in 0..256 {
                for j in 0..512 {
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
                // when the gui starts a test pattern is loaded into the texture
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
                    (256 as i32),
                    (512 as i32),
                    0,
                    gl::RGB,
                    gl::UNSIGNED_BYTE,
                    ((&(data[0]) as *const u8) as *const c_void),
                );
            };
            let mut imgui = imgui::Context::create();
            let mut imgui_glfw = imgui_glfw_rs::ImguiGLFW::new(&mut imgui, &mut window);
            let mut line_yoffset = 0;
            let mut buffer_fill = 0.;
            imgui.set_ini_filename(None);
            while (!(window.should_close())) {
                let v: Vec<_> = r2.try_iter().collect();
                buffer_fill = (((1.00e+2) * (v.len() as f32)) / (40.));
                // each response received on r2 is a line that will be written into the texture
                // v.len() should never become 40 this would mean that the s2 channel is full and back pressure would lead to dropped lines. if v.len()
                for c in v {
                    let cc: usize = c;
                    let hb = fftout_scaled[cc].clone();
                    let b = &hb.lock().unwrap();
                    unsafe {
                        gl::TexSubImage2D(
                            gl::TEXTURE_2D,
                            0,
                            0,
                            line_yoffset,
                            256,
                            1,
                            gl::RED,
                            gl::FLOAT,
                            ((&(b[0]) as *const f32) as *const c_void),
                        );
                    }
                    line_yoffset += 1;
                    if (512) <= (line_yoffset) {
                        line_yoffset = 0;
                    };
                }
                unsafe {
                    gl::Clear(((gl::COLOR_BUFFER_BIT) | (gl::DEPTH_BUFFER_BIT)));
                }
                {
                    let ui = imgui_glfw.frame(&mut window, &mut imgui);
                    ui.show_metrics_window(&mut true);
                    imgui::Window::new(&ui, im_str!("waterfall fft")).build(|| {
                        ui.text(im_str!("buffer_fill={:?}%", buffer_fill));
                        ui.image(texture_id, [256., 512.]).build();
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
                            {
                                println!(
                                    "{} {}:{} gui wants to quit, notify all threads ",
                                    Utc::now(),
                                    file!(),
                                    line!()
                                );
                            }
                            keep_running.swap(false, std::sync::atomic::Ordering::Relaxed);
                            window.set_should_close(true);
                        }
                        _ => {}
                    }
                }
            }
        });
        scope.builder().name("fft_scaler".into()).spawn(|_| {
            let wg = barrier_pipeline_setup.clone();
            {
                println!(
                    "{} {}:{} fft_scaler waits for other pipeline threads ",
                    Utc::now(),
                    file!(),
                    line!()
                );
            }
            wg.wait();
            {
                println!(
                    "{} {}:{} fft_scaler loop starts ",
                    Utc::now(),
                    file!(),
                    line!()
                );
            }
            let mut count = 0;
            while (keep_running.load(std::sync::atomic::Ordering::Relaxed)) {
                let tup: usize = r1.recv().ok().unwrap();
                let mut hc = fftout_scaled[count].clone();
                let mut c = &mut hc.lock().unwrap();
                let hb = fftout[tup].clone();
                let b = &hb.lock().unwrap();
                let scale = ((23.) / (256 as f32));
                let offset = (-0.90);
                // convert complex fft results to log of magnitude, apply scale and offset and preform fftshift
                for i in 0..128 {
                    c[(i + 127)] = (offset
                        + ((scale)
                            * ((((b.ptr[i].re) * (b.ptr[i].re)) + ((b.ptr[i].im) * (b.ptr[i].im)))
                                .ln() as f32)));
                }
                for i in 0..128 {
                    let j = (i + 128);
                    c[i] = (offset
                        + ((scale)
                            * ((((b.ptr[j].re) * (b.ptr[j].re)) + ((b.ptr[j].im) * (b.ptr[j].im)))
                                .ln() as f32)));
                }
                s2.send(count).unwrap();
                count += 1;
                if (40) <= (count) {
                    count = 0;
                };
            }
        });
        scope.builder().name("fft_processor".into()).spawn(|_| {
            let wg = barrier_pipeline_setup.clone();
            {
                println!("{} {}:{} start fftw plan ", Utc::now(), file!(), line!());
            }
            let mut plan: fftw::plan::C2CPlan64 = fftw::plan::C2CPlan::aligned(
                &[256],
                fftw::types::Sign::Forward,
                fftw::types::Flag::Measure,
            )
            .unwrap();
            {
                println!("{} {}:{} finish fftw plan ", Utc::now(), file!(), line!());
            }
            {
                println!(
                    "{} {}:{} fft_processor waits for other pipeline threads ",
                    Utc::now(),
                    file!(),
                    line!()
                );
            }
            wg.wait();
            {
                println!(
                    "{} {}:{} fft_processor loop starts ",
                    Utc::now(),
                    file!(),
                    line!()
                );
            }
            while (keep_running.load(std::sync::atomic::Ordering::Relaxed)) {
                let tup: usize = r0.recv().ok().unwrap();
                let mut ha = fftin[tup].clone();
                let mut a = &mut ha.lock().unwrap();
                let mut hb = fftout[tup].clone();
                let mut b = &mut hb.lock().unwrap();
                plan.c2c(&mut a.ptr, &mut b.ptr).unwrap();
                b.timestamp = Utc::now();
                s1.send(tup).unwrap();
            }
        });
        scope.builder().name("sdr_reader".into()).spawn(|_| {
            let wg = barrier_pipeline_setup.clone();
            // i start my linux with the kernel parameter isolcpus=0,1
            // the sdr_reader thread is the only process in this core
            // i'm not sure if that helps at all against underflow. perhaps the usb communication is handled in the kernel which will then run on slightly busier cores
            // i keep it in in case i ever get this program compiled for the embedded arm processor on the zynq in the pluto
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
                        "{} {}:{} 16 bit channels found  nchan={:?}",
                        Utc::now(),
                        file!(),
                        line!(),
                        nchan
                    );
                }
            };
            let mut chans = Vec::new();
            let mut buf = dev.create_buffer(256, false).unwrap_or_else(|err| {
                {
                    println!(
                        "{} {}:{} can't create buffer  err={:?}",
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
            let mut count = 0;
            {
                println!(
                    "{} {}:{} sdr_reader waits for other pipeline threads ",
                    Utc::now(),
                    file!(),
                    line!()
                );
            }
            wg.wait();
            {
                println!(
                    "{} {}:{} sdr_reader loop starts ",
                    Utc::now(),
                    file!(),
                    line!()
                );
            }
            while (keep_running.load(std::sync::atomic::Ordering::Relaxed)) {
                match buf.refill() {
                    Err(err) => {
                        {
                            println!(
                                "{} {}:{} error filling buffer  err={:?}",
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
                    for i in 0..256 {
                        a.ptr[i] = fftw::types::c64::new((data_i[i] as f64), (data_q[i] as f64));
                    }
                }
                s0.send(count).unwrap();
                count += 1;
                if (4) <= (count) {
                    count = 0;
                };
            }
        });
    });
}
