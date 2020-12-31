use std::rc::Rc;
use std::unreachable;
use std::{cell::RefCell, fmt::Display};

use crossterm::{queue, style::Print};

type RR<T> = Rc<RefCell<T>>;

struct Box<'a>(RR<_Box<'a>>);
struct _Box<'a> {
    orientation: Orientation,
    children: Vec<&'a dyn Widget<'a>>,
}
impl<'a> Widget<'a> for Box<'a> {
    fn downcast(&self) -> Type<'a> {
        Type::Box(self.clone())
    }
    fn text(&'a self) -> &'static str {
        unreachable!()
    }
}
impl Clone for Box<'_> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<'a> Box<'a> {
    fn new(orientation: Orientation) -> Self {
        Self(Rc::new(RefCell::new(_Box {
            orientation,
            children: vec![],
        })))
    }
    fn add(&self, widget: &'a dyn Widget<'a>) {
        self.0.borrow_mut().children.push(widget);
    }

    fn get_children(&self) -> Vec<&'a dyn Widget> {
        self.0
            .borrow()
            .children
            .iter()
            .map(ToOwned::to_owned)
            .collect()
    }
}
enum Orientation {
    Horizontal,
    Vertical,
}
struct Label(RR<_Label>);
struct _Label {
    label: &'static str,
}
impl Label {
    fn new(label: &'static str) -> Self {
        Self(Rc::new(RefCell::new(_Label { label })))
    }
    fn set_text(&self, text: &'static str) {
        self.0.borrow_mut().label = text;
    }
}
impl Clone for Label {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<'a> Widget<'a> for Label {
    fn downcast(&'a self) -> Type<'a> {
        Type::Label(self.clone())
    }
    fn text(&'a self) -> &'static str {
        self.0.borrow().label
    }
}

struct Button(RR<_Button>);
struct _Button {
    label: &'static str,
}
impl Button {
    fn new(label: &'static str) -> Self {
        Self(Rc::new(RefCell::new(_Button { label })))
    }
    fn connect_clicked<F: Fn(&Self)>(&self, fun: F) {
        fun(self);
    }
}
impl Clone for Button {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<'a> Widget<'a> for Button {
    fn downcast(&'a self) -> Type<'a> {
        Type::Button(self.clone())
    }
    fn text(&'a self) -> &'static str {
        self.0.borrow().label
    }
}
trait Widget<'a> {
    fn downcast(&'a self) -> Type<'a>;
    fn text(&'a self) -> &'static str;
}

enum Type<'a> {
    Box(Box<'a>),
    Window(Window<'a>),
    Label(Label),
    Button(Button),
}

struct Window<'a>(RR<_Window<'a>>);
struct _Window<'a> {
    child: Vec<&'a dyn Widget<'a>>,
    size: (usize, usize),
}
impl<'a> Window<'a> {
    fn new() -> Self {
        let (w, h) = crossterm::terminal::size().unwrap();
        let size = (w as usize, h as usize);
        Self(Rc::new(RefCell::new(_Window {
            child: vec![],
            size,
        })))
    }
    fn add(&self, child: &'a dyn Widget<'a>) {
        self.0.borrow_mut().child.insert(0, child);
    }
}

impl Clone for Window<'_> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<'a> Widget<'a> for Window<'a> {
    fn downcast(&'a self) -> Type<'a> {
        Type::Window(self.clone())
    }
    fn text(&'a self) -> &'static str {
        unreachable!()
    }
}

fn draw<'a>(widget: &'a dyn Widget<'a>, size: Option<(usize, usize)>) -> ! {
    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();

    match widget.downcast() {
        Type::Box(tbox) => {
            let children = tbox.get_children();
            let num = children.len();
            for child in children {
                queue!(stdout, Print(child.text()));
            }
        }
        Type::Window(win) => draw(win.0.borrow().child[0], Some(win.0.borrow().size)),
        Type::Label(label) => {}
        Type::Button(btn) => {}
    }
    loop {}
}

mod tests {
    use super::*;
    #[test]
    fn layout() {
        let win = Window::new();
        let hbox = Box::new(Orientation::Horizontal);
        let label = Label::new("hello");
        let label_c = label.clone();
        let btn = Button::new("make world");
        btn.connect_clicked(|_btn| {
            label_c.set_text("world");
        });
        hbox.add(&label);
        hbox.add(&btn);
        win.add(&hbox);

        draw(&win, None);
    }
}
