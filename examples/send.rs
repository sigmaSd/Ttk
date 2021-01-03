use std::{cell::RefCell, rc::Rc};

use crossterm::{cursor::MoveTo, queue, style::Print};
use ttk::{Area, Box, Button, Container, Request, Widget, Window};
fn main() {
    let vbox = Box::new(ttk::Orientation::Vertical);
    let send_btn = Button::new("Send".into());
    let progress = Progress::new(100);
    let p_c = progress.clone();

    let (tx, mut rx) = ttk::channel();
    send_btn.connect_clicked(move || {
        let tx = tx.clone();
        std::thread::spawn(move || {
            for _ in 0..=100 {
                std::thread::sleep(std::time::Duration::from_millis(100));
                tx.send(()).unwrap();
            }
        });
    });

    let win = Window::new();
    vbox.add(send_btn);
    vbox.add(progress);
    win.add(vbox);

    rx.attach(std::boxed::Box::new(move |_| {
        p_c.borrow_mut().tick();
    }));

    ttk::main(&win, Some(rx));
}

#[derive(Debug)]
struct Progress {
    max: usize,
    current: usize,
    active: Rc<RefCell<bool>>,
}

impl Progress {
    fn new(max: usize) -> Rc<RefCell<Progress>> {
        Rc::new(RefCell::new(Progress {
            max,
            current: 0,
            active: Rc::new(RefCell::new(false)),
        }))
    }
    fn tick(&mut self) {
        self.current += self.step();
        if self.current > self.max {
            self.current = self.max;
        }
    }
    fn step(&self) -> usize {
        self.max / 100
    }
    fn current_percent(&self) -> f32 {
        self.current as f32 / self.max as f32
    }
    fn finished(&self) -> bool {
        self.current == self.max
    }
}

impl Widget for Progress {
    fn draw(&self, stdout: &mut std::io::StdoutLock, area: Area) -> Request {
        let width = area.width() - 2; // account for [ ]
        let width_f32 = width as f32;
        let current = (self.current_percent() * width_f32) as usize;
        let remaining = width - current;

        let current: String = std::iter::repeat('#').take(current).collect();
        let remaining: String = std::iter::repeat('-').take(remaining).collect();

        let bar = format!("[{}{}]", current, remaining);
        queue!(
            stdout,
            MoveTo(area.left_up().0 as u16, area.left_up().1 as u16,)
        )
        .unwrap();
        queue!(stdout, Print(bar)).unwrap();

        if self.finished() {
            Request::None
        } else {
            Request::Redraw
        }
    }
    fn get_active_state(&self) -> Rc<RefCell<bool>> {
        self.active.clone()
    }
}
