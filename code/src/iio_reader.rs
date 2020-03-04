#[allow(unused_parens)]
use chrono::{DateTime, Utc};
use crossbeam_channel::bounded;
use fftw;
use fftw::plan::C2CPlan;
use std::io;
use std::sync::{Arc, Mutex};
use std::thread::spawn;
pub struct SendComplex {
    pub timestamp: DateTime<Utc>,
    pub ptr: fftw::array::AlignedVec<num_complex::Complex<f64>>,
}
unsafe impl Send for SendComplex {}
pub fn iio_read(
    fftin: [Arc<Mutex<SendComplex>>; 3],
    fftout: [Arc<Mutex<SendComplex>>; 3],
    send_to_fft_scaler: crossbeam_channel::Sender<usize>,
) {
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
        let (s, r) = crossbeam_channel::bounded(3);
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
                    let tup: usize = r.recv().ok().unwrap();
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
                    send_to_fft_scaler.send(tup).unwrap();
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
                s.send(count).unwrap();
                count += 1;
                if (3) <= (count) {
                    count = 0;
                };
            }
        })
        .unwrap();
    });
}
