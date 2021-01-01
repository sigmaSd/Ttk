use std::cell::RefCell;
use std::io::Write;
use std::unreachable;
use std::{ops::Range, rc::Rc};

use crossterm::{
    cursor::{Hide, MoveDown, MoveTo, MoveToColumn, MoveToNextLine, MoveUp, Show},
    event::{DisableMouseCapture, EnableMouseCapture, Event, MouseEventKind},
    queue,
    style::Print,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen},
};

type RR<T> = Rc<RefCell<T>>;

pub struct Box(RR<_Box>);
struct _Box {
    orientation: Orientation,
    children: Vec<&'static dyn Widget>,
}
impl Widget for Box {
    fn downcast(&self) -> Type {
        Type::Box(self.clone())
    }
    fn text(&self) -> &'static str {
        unreachable!()
    }
}
impl Clone for Box {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl Box {
    pub fn new(orientation: Orientation) -> Self {
        Self(Rc::new(RefCell::new(_Box {
            orientation,
            children: vec![],
        })))
    }
    pub fn add<'a>(&self, widget: &'a dyn Widget) {
        let widget = unsafe { std::mem::transmute(widget) };
        self.0.borrow_mut().children.push(widget);
    }

    fn get_children(&self) -> Vec<&'static dyn Widget> {
        self.0
            .borrow()
            .children
            .iter()
            .map(ToOwned::to_owned)
            .collect()
    }
}
pub enum Orientation {
    Horizontal,
    Vertical,
}
pub struct Label(RR<_Label>);
struct _Label {
    label: &'static str,
}
impl Label {
    pub fn new(label: &'static str) -> Self {
        Self(Rc::new(RefCell::new(_Label { label })))
    }
    pub fn set_text(&self, text: &'static str) {
        self.0.borrow_mut().label = text;
    }
}
impl Clone for Label {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl Widget for Label {
    fn downcast(&'static self) -> Type {
        Type::Label(self.clone())
    }
    fn text(&self) -> &'static str {
        self.0.borrow().label
    }
}

pub struct Button(RR<_Button>);
struct _Button {
    label: &'static str,
    signal: Option<std::boxed::Box<dyn Fn()>>,
}
impl Button {
    pub fn new(label: &'static str) -> Self {
        Self(Rc::new(RefCell::new(_Button {
            label,
            signal: None,
        })))
    }
    pub fn connect_clicked<F: Fn() + 'static>(&self, fun: F) {
        self.0.borrow_mut().signal = Some(std::boxed::Box::new(fun));
    }
    fn click(&self) {
        (self.0.borrow().signal.as_ref().unwrap())();
    }
}
impl Clone for Button {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl Widget for Button {
    fn downcast(&'static self) -> Type {
        Type::Button(self.clone())
    }
    fn text(&self) -> &'static str {
        self.0.borrow().label
    }
}
pub trait Widget {
    fn downcast(&'static self) -> Type;
    fn text(&self) -> &'static str;
}

pub enum Type {
    Box(Box),
    Window(Window),
    Label(Label),
    Button(Button),
    Entry(Entry),
    List(List),
}
impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Box(_) => write!(f, "box"),
            Type::Window(_) => write!(f, "win"),
            Type::Label(_) => write!(f, "label"),
            Type::Button(_) => write!(f, "btn"),
            Type::Entry(_) => write!(f, "entry"),
            Type::List(_) => write!(f, "list"),
        }
    }
}

pub struct Window(RR<_Window>);
struct _Window {
    child: Vec<&'static dyn Widget>,
    size: (usize, usize),
}
impl Window {
    pub fn new() -> Self {
        let (w, h) = crossterm::terminal::size().unwrap();
        let size = (w as usize, h as usize);
        Self(Rc::new(RefCell::new(_Window {
            child: vec![],
            size,
        })))
    }
    pub fn add<'a>(&self, child: &'a dyn Widget) {
        let child = unsafe { std::mem::transmute(child) };
        self.0.borrow_mut().child.insert(0, child);
    }
}

impl Clone for Window {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl Widget for Window {
    fn downcast(&'static self) -> Type {
        Type::Window(self.clone())
    }
    fn text(&self) -> &'static str {
        unreachable!()
    }
}

pub struct Entry(RR<_Entry>);
struct _Entry {
    buffer: String,
    active: bool,
    signal: Option<std::boxed::Box<dyn Fn(&Entry)>>,
}
impl Entry {
    pub fn new() -> Self {
        Self(Rc::new(RefCell::new(_Entry {
            buffer: String::new(),
            active: false,
            signal: None,
        })))
    }
    pub fn connect_changed<F: Fn(&Self) + 'static>(&self, fun: F) {
        self.0.borrow_mut().signal = Some(std::boxed::Box::new(fun));
    }
    fn push(&self, c: char) {
        self.0.borrow_mut().buffer.push(c);
        (self.0.borrow().signal.as_ref().unwrap())(self);
    }
    fn pop(&self) {
        self.0.borrow_mut().buffer.pop();
        (self.0.borrow().signal.as_ref().unwrap())(self);
    }
    pub fn get_text(&self) -> &'static str {
        unsafe { std::mem::transmute(self.0.borrow().buffer.as_str()) }
    }
}
impl Clone for Entry {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl Widget for Entry {
    fn downcast(&'static self) -> Type {
        Type::Entry(self.clone())
    }
    fn text(&self) -> &'static str {
        unsafe { std::mem::transmute(self.0.borrow().buffer.as_str()) }
    }
}

enum TEvent {
    MouseClick((usize, usize)),
    Nop,
    Key(Key),
}
enum Key {
    Char(char),
    Backspace,
}

fn draw(win: &Window, rx: &std::sync::mpsc::Receiver<Event>) -> bool {
    let size = win.0.borrow().size;
    let size = (0..size.0, 0..size.1);
    let widget = win.0.borrow().child[0];

    // draw once atleast
    draw_inner(widget, size.clone(), &TEvent::Nop);
    // flush happens here each frame
    std::io::stdout().flush().unwrap();

    match rx.recv() {
        Ok(ev) => match ev {
            crossterm::event::Event::Mouse(m) => {
                if let MouseEventKind::Down(_) = m.kind {
                    draw_inner(
                        widget,
                        size,
                        &TEvent::MouseClick((m.column as usize, m.row as usize)),
                    );
                }
            }
            crossterm::event::Event::Key(crossterm::event::KeyEvent {
                code: crossterm::event::KeyCode::Char(c),
                modifiers: crossterm::event::KeyModifiers::CONTROL,
            }) => {
                if c == 'c' {
                    return false;
                }
            }
            crossterm::event::Event::Key(crossterm::event::KeyEvent {
                code: crossterm::event::KeyCode::Char(c),
                ..
            }) => {
                draw_inner(widget, size, &TEvent::Key(Key::Char(c)));
            }
            crossterm::event::Event::Key(crossterm::event::KeyEvent {
                code: crossterm::event::KeyCode::Backspace,
                ..
            }) => {
                draw_inner(widget, size, &TEvent::Key(Key::Backspace));
            }
            _ => (),
        },
        Err(_) => {}
    }
    true
}

fn draw_inner(widget: &dyn Widget, size: (Range<usize>, Range<usize>), event: &TEvent) {
    let widget: &'static dyn Widget = unsafe { std::mem::transmute(widget) };
    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();

    match widget.downcast() {
        Type::Box(tbox) => match tbox.0.borrow().orientation {
            Orientation::Horizontal => {
                let children = tbox.get_children();
                let num = children.len();

                let (x, y) = size;

                let step = (x.end - x.start) / num;

                let ranges: Vec<_> = div(x, num).into_iter().map(|r| (r, y.clone())).collect();

                for (idx, (child, range)) in
                    children.into_iter().zip(ranges.into_iter()).enumerate()
                {
                    queue!(stdout, MoveToColumn((idx * step) as u16)).unwrap();
                    draw_inner(child, range, &event);
                }
            }
            Orientation::Vertical => {
                let children = tbox.get_children();
                let num = children.len();

                let (x, y) = size;

                let step = (y.end - y.start) / num;

                let ranges: Vec<_> = div(y, num).into_iter().map(|r| (x.clone(), r)).collect();

                for (idx, (child, range)) in
                    children.into_iter().zip(ranges.into_iter()).enumerate()
                {
                    //todo add movetorow
                    queue!(stdout, MoveUp(500)).unwrap();
                    queue!(stdout, MoveDown((idx * step) as u16)).unwrap();
                    draw_inner(child, range, event);
                }
            }
        },
        Type::Label(label) => {
            queue!(stdout, Print(label.text())).unwrap();
        }
        Type::Button(btn) => {
            if let TEvent::MouseClick(pos) = event {
                if size.0.contains(&pos.0) && size.1.contains(&pos.1) {
                    btn.click();
                }
            }
            queue!(stdout, Print(btn.text())).unwrap();
        }
        Type::Entry(ent) => {
            match event {
                TEvent::MouseClick(pos) => {
                    if size.0.contains(&pos.0) && size.1.contains(&pos.1) {
                        ent.0.borrow_mut().active = true;
                        queue!(stdout, MoveTo(size.0.start as u16, size.1.start as u16)).unwrap();
                    } else {
                        ent.0.borrow_mut().active = false;
                    }
                }
                TEvent::Key(Key::Char(c)) if ent.0.borrow().active => {
                    ent.push(*c);
                }
                TEvent::Key(Key::Backspace) if ent.0.borrow().active => {
                    ent.pop();
                }
                _ => (),
            }
            queue!(stdout, Print(ent.text())).unwrap();
        }
        Type::List(list) => {
            let children = list.get_children();

            let num = children.len();
            let (x, y) = size;

            let child_area: Vec<_> = div(y, num).into_iter().map(|s| (x.clone(), s)).collect();

            queue!(stdout, crossterm::cursor::MoveToColumn(0)).unwrap();
            for (child, area) in children.into_iter().zip(child_area.into_iter()) {
                draw_inner(child, area, event);
                queue!(stdout, MoveToNextLine(1)).unwrap();
            }
        }
        _ => unreachable!(),
    }
}
pub struct List(RR<_List>);
struct _List {
    items: Vec<&'static dyn Widget>,
    filter: Option<&'static str>,
}
impl Widget for List {
    fn downcast(&'static self) -> Type {
        Type::List(self.clone())
    }
    fn text(&self) -> &'static str {
        unreachable!()
    }
}
impl Clone for List {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl List {
    pub fn new() -> Self {
        Self(Rc::new(RefCell::new(_List {
            items: vec![],
            filter: None,
        })))
    }
    fn get_children(&self) -> Vec<&'static dyn Widget> {
        if let Some(filter) = self.0.borrow().filter {
            self.0
                .borrow()
                .items
                .iter()
                .filter(|i| i.text().contains(filter))
                .map(ToOwned::to_owned)
                .collect()
        } else {
            self.0
                .borrow()
                .items
                .iter()
                .map(ToOwned::to_owned)
                .collect()
        }
    }
    pub fn add<'a>(&self, widget: &'a dyn Widget) {
        let widget: &'static dyn Widget = unsafe { std::mem::transmute(widget) };
        self.0.borrow_mut().items.push(widget);
    }
    pub fn set_filter(&self, filter: &'static str) {
        self.0.borrow_mut().filter = Some(filter);
    }
}

pub fn main(win: &Window) {
    crossterm::terminal::enable_raw_mode().unwrap();
    crossterm::queue!(std::io::stdout(), EnterAlternateScreen).unwrap();
    crossterm::queue!(std::io::stdout(), EnableMouseCapture).unwrap();
    crossterm::queue!(std::io::stdout(), Hide).unwrap();
    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || loop {
        tx.send(crossterm::event::read().unwrap()).unwrap();
    });
    loop {
        queue!(std::io::stdout(), MoveTo(0, 0)).unwrap();
        queue!(
            std::io::stdout(),
            crossterm::terminal::Clear(crossterm::terminal::ClearType::All)
        )
        .unwrap();
        if !draw(&win, &rx) {
            break;
        }
    }
    crossterm::terminal::disable_raw_mode().unwrap();
    crossterm::queue!(std::io::stdout(), LeaveAlternateScreen).unwrap();
    crossterm::queue!(std::io::stdout(), DisableMouseCapture).unwrap();
    crossterm::queue!(std::io::stdout(), Show).unwrap();
}

fn div(r: std::ops::Range<usize>, d: usize) -> Vec<Range<usize>> {
    if d == 0 || d == 1 {
        return vec![r];
    }
    let mut v = vec![];

    let mut start = r.start;
    let step = (r.end - r.start) / d;
    for _ in 0..d {
        let end = start + step;
        v.push(start..end);
        start = end;
    }
    v
}
