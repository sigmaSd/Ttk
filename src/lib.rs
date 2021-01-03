use crossterm::{
    cursor::{Hide, MoveDown, MoveTo, MoveToColumn, MoveToNextLine, MoveUp, Show},
    event::{DisableMouseCapture, EnableMouseCapture, MouseEventKind},
    queue,
    style::Print,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen},
};
use std::cell::RefCell;
use std::io::Write;
use std::sync::mpsc::{self, Receiver, Sender};
use std::{ops::Range, rc::Rc};
type RR<T> = Rc<RefCell<T>>;

//*******Widgets*************

//****Widget Trait****
pub trait Widget {
    fn text(&self) -> &str {
        "Widget"
    }
    fn draw(&self, stdout: &mut std::io::StdoutLock, area: Area) -> Request {
        let text = self.text();
        let width = area.width();
        if text.len() > width {
            crossterm::queue!(
                stdout,
                Print(String::from_utf8_lossy(&text.as_bytes()[..width]))
            )
            .unwrap();
        } else {
            crossterm::queue!(stdout, Print(text)).unwrap();
        }
        Request::None
    }
    fn event(&self, _event: TEvent, _area: Area) -> Request {
        Request::None
    }
    fn get_active_state(&self) -> Rc<RefCell<bool>>;
    fn activate(&self, _area: Area) {
        *self.get_active_state().borrow_mut() = true;
    }
    fn deactivate(&self, _area: Area) {
        *self.get_active_state().borrow_mut() = false;
    }
    fn is_active(&self) -> bool {
        *self.get_active_state().borrow()
    }
}
// Request a widget can make
pub enum Request {
    Redraw,
    None,
}
impl Request {
    fn merge(&mut self, other: Request) {
        #[allow(clippy::single_match)]
        match (self, other) {
            (s, Self::Redraw) => *s = Self::Redraw,
            _ => (),
        }
    }
}

impl<T> Widget for RefCell<T>
where
    T: Widget,
{
    fn text(&self) -> &str {
        std::boxed::Box::leak(std::boxed::Box::new(self.borrow().text().to_string()))
    }
    fn draw(&self, stdout: &mut std::io::StdoutLock, area: Area) -> Request {
        self.borrow().draw(stdout, area)
    }
    fn event(&self, event: TEvent, area: Area) -> Request {
        self.borrow().event(event, area)
    }
    fn get_active_state(&self) -> Rc<RefCell<bool>> {
        self.borrow().get_active_state()
    }
    fn activate(&self, area: Area) {
        self.borrow().activate(area);
    }
    fn deactivate(&self, area: Area) {
        self.borrow().deactivate(area);
    }

    fn is_active(&self) -> bool {
        self.borrow().is_active()
    }
}

//**Container Trait **/
pub trait Container: Widget {
    fn add(&self, widget: Rc<dyn Widget>);
    fn get_children(&self) -> Vec<Rc<dyn Widget>>;
    fn get_children_area(&self, area: Area) -> Vec<Area>;
    fn get_child(&self, n: usize) -> Rc<dyn Widget> {
        self.get_children().remove(n)
    }
    fn get_children_num(&self) -> usize {
        self.get_children().into_iter().count()
    }
    fn clear(&self);
    fn propagate_event(&self, event: TEvent, area: Area) -> Request;
}

//**Filterable trait**/
pub trait Filterable: Container {
    fn get_children_with_filter(&self, filter: &str) -> Vec<Rc<dyn Widget>>;
}

//***Area */
//The area the widget can be drawn on
// Also serves to catch signals
#[derive(Clone, Debug)]
pub struct Area {
    horizontal: Range<usize>,
    vertical: Range<usize>,
}
impl Area {
    pub fn width(&self) -> usize {
        self.horizontal.end - self.horizontal.start
    }
    fn height(&self) -> usize {
        self.vertical.end - self.vertical.start
    }
    fn split_horizontal(self, n: usize) -> Vec<Area> {
        div(self.horizontal.clone(), n)
            .into_iter()
            .map(|new_horizontal| Area {
                horizontal: new_horizontal,
                vertical: self.vertical.clone(),
            })
            .collect()
    }
    fn split_vertical(self, n: usize) -> Vec<Area> {
        div(self.vertical.clone(), n)
            .into_iter()
            .map(|new_vertical| Area {
                horizontal: self.horizontal.clone(),
                vertical: new_vertical,
            })
            .collect()
    }
    fn contains(&self, pos: &(usize, usize)) -> bool {
        self.horizontal.contains(&pos.0) && self.vertical.contains(&pos.1)
    }
    pub fn left_up(&self) -> (usize, usize) {
        (self.horizontal.start, self.vertical.start)
    }
}
impl From<(Range<usize>, Range<usize>)> for Area {
    fn from(area: (Range<usize>, Range<usize>)) -> Self {
        Area {
            horizontal: area.0,
            vertical: area.1,
        }
    }
}
impl From<(usize, usize)> for Area {
    fn from(area: (usize, usize)) -> Self {
        Area {
            horizontal: 0..area.0,
            vertical: 0..area.1,
        }
    }
}

//****Actual Widgets*** */
//**Box**
pub struct Box(RR<_Box>, Rc<RefCell<bool>>);
struct _Box {
    orientation: Orientation,
    children: Vec<Rc<dyn Widget>>,
}
impl Widget for Box {
    fn get_active_state(&self) -> Rc<RefCell<bool>> {
        self.1.clone()
    }
    fn draw(&self, stdout: &mut std::io::StdoutLock, area: Area) -> Request {
        let mut request = Request::None;
        match self.get_orientation() {
            Orientation::Horizontal => {
                let children = self.get_children();
                let step = area.width() / self.get_children_num();
                let children_area = self.get_children_area(area);

                for (idx, (child, area)) in children
                    .into_iter()
                    .zip(children_area.into_iter())
                    .enumerate()
                {
                    queue!(stdout, MoveToColumn((idx * step) as u16)).unwrap();
                    request.merge(child.draw(stdout, area));
                }
                request
            }
            Orientation::Vertical => {
                let children = self.get_children();
                let step = area.height() / self.get_children_num();
                let children_area = self.get_children_area(area);

                for (idx, (child, area)) in children
                    .into_iter()
                    .zip(children_area.into_iter())
                    .enumerate()
                {
                    //todo add movetorow
                    queue!(stdout, MoveUp(500)).unwrap();
                    queue!(stdout, MoveDown((idx * step) as u16)).unwrap();
                    request.merge(child.draw(stdout, area));
                }
                request
            }
        }
    }
    fn event(&self, event: TEvent, area: Area) -> Request {
        self.propagate_event(event, area)
    }
}
impl Clone for Box {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}

impl Box {
    pub fn new(orientation: Orientation) -> Rc<Self> {
        Rc::new(Self(
            Rc::new(RefCell::new(_Box {
                orientation,
                children: vec![],
            })),
            Rc::new(RefCell::new(false)),
        ))
    }
    pub fn get_orientation(&self) -> Orientation {
        self.0.borrow().orientation
    }
}

impl Container for Box {
    fn add(&self, widget: Rc<dyn Widget>) {
        self.0.borrow_mut().children.push(widget);
    }
    fn get_children(&self) -> Vec<Rc<dyn Widget>> {
        self.0.borrow().children.to_vec()
    }
    fn get_children_num(&self) -> usize {
        self.0.borrow().children.iter().count()
    }
    fn get_children_area(&self, area: Area) -> Vec<Area> {
        let num = self.get_children_num();
        match self.get_orientation() {
            Orientation::Vertical => area.split_vertical(num),
            Orientation::Horizontal => area.split_horizontal(num),
        }
    }
    fn clear(&self) {
        self.0.borrow_mut().children.clear();
    }
    fn propagate_event(&self, event: TEvent, area: Area) -> Request {
        let children = self.get_children();
        let children_area = self.get_children_area(area);
        let mut request = Request::None;
        children
            .into_iter()
            .zip(children_area.into_iter())
            .for_each(|(child, area)| {
                request.merge(child.event(event, area));
            });
        request
    }
}
#[derive(Clone, Copy)]
pub enum Orientation {
    Horizontal,
    Vertical,
}

//**Label**
pub struct Label(RR<_Label>, Rc<RefCell<bool>>);
struct _Label {
    label: &'static str,
}
impl Label {
    pub fn new(label: &'static str) -> Rc<Self> {
        Rc::new(Self(
            Rc::new(RefCell::new(_Label { label })),
            Rc::new(RefCell::new(false)),
        ))
    }
    pub fn set_text(&self, text: &'static str) {
        self.0.borrow_mut().label = text;
    }
}
impl Clone for Label {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}
impl Widget for Label {
    fn text(&self) -> &'static str {
        self.0.borrow().label
    }
    fn get_active_state(&self) -> Rc<RefCell<bool>> {
        self.1.clone()
    }
}

//**Button**
pub struct Button(RR<_Button>, Rc<RefCell<bool>>);
struct _Button {
    label: String,
    signal: Option<std::boxed::Box<dyn Fn()>>,
}
impl Button {
    pub fn new(label: String) -> Rc<Self> {
        Rc::new(Self(
            Rc::new(RefCell::new(_Button {
                label,
                signal: None,
            })),
            Rc::new(RefCell::new(false)),
        ))
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
        Self(self.0.clone(), self.1.clone())
    }
}

impl Widget for Button {
    fn text(&self) -> &'static str {
        std::boxed::Box::leak(std::boxed::Box::new(self.0.borrow().label.clone()))
    }
    fn get_active_state(&self) -> Rc<RefCell<bool>> {
        self.1.clone()
    }
    fn event(&self, event: TEvent, area: Area) -> Request {
        if let TEvent::MouseClick(pos) = event {
            if area.contains(&pos) {
                self.click();
                //todo this is currently unused
                // but its logically the correct behavior
                return Request::Redraw;
            }
        }
        Request::None
    }
}

//**Window**
pub struct Window(RR<_Window>, Rc<RefCell<bool>>);
struct _Window {
    child: Vec<Rc<dyn Widget>>,
    area: Area,
}
impl Window {
    pub fn new() -> Rc<Self> {
        let (w, h) = crossterm::terminal::size().unwrap();
        let area = (w as usize, h as usize).into();
        Rc::new(Self(
            Rc::new(RefCell::new(_Window {
                child: vec![],
                area,
            })),
            Rc::new(RefCell::new(true)),
        ))
    }
    fn set_area(&self, area: Area) {
        self.0.borrow_mut().area = area;
    }
    fn get_area(&self) -> Area {
        self.0.borrow().area.clone()
    }
}

impl Clone for Window {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}
impl Widget for Window {
    fn get_active_state(&self) -> Rc<RefCell<bool>> {
        self.1.clone()
    }
    fn event(&self, event: TEvent, area: Area) -> Request {
        self.propagate_event(event, area)
    }
}
impl Container for Window {
    fn add(&self, widget: Rc<dyn Widget>) {
        self.0.borrow_mut().child.push(widget);
    }
    fn get_children(&self) -> Vec<Rc<dyn Widget>> {
        self.0.borrow().child.to_vec()
    }
    fn clear(&self) {
        self.0.borrow_mut().child.clear();
    }
    fn propagate_event(&self, event: TEvent, area: Area) -> Request {
        self.get_child(0).event(event, area)
    }
    fn get_children_area(&self, _area: Area) -> Vec<Area> {
        vec![self.get_area()]
    }
}

//**Entry**
pub struct Entry(RR<_Entry>, Rc<RefCell<bool>>);
struct _Entry {
    buffer: String,
    changed_signal: Option<std::boxed::Box<dyn Fn(&Entry)>>,
    enter_signal: Option<std::boxed::Box<dyn Fn(&Entry)>>,
}
impl Entry {
    pub fn new() -> Rc<Self> {
        Rc::new(Entry(
            Rc::new(RefCell::new(_Entry {
                buffer: String::new(),
                changed_signal: None,
                enter_signal: None,
            })),
            Rc::new(RefCell::new(false)),
        ))
    }
    pub fn connect_changed<F: Fn(&Self) + 'static>(&self, fun: F) {
        self.0.borrow_mut().changed_signal = Some(std::boxed::Box::new(fun));
    }
    pub fn connect_enter<F: Fn(&Self) + 'static>(&self, fun: F) {
        self.0.borrow_mut().enter_signal = Some(std::boxed::Box::new(fun));
    }
    fn push(&self, c: char) {
        self.0.borrow_mut().buffer.push(c);
        if let Some(sig) = self.0.borrow().changed_signal.as_ref() {
            sig(self)
        }
    }
    fn pop(&self) {
        self.0.borrow_mut().buffer.pop();
        if let Some(sig) = self.0.borrow().changed_signal.as_ref() {
            sig(self)
        }
    }
    pub fn get_text(&self) -> &'static str {
        std::boxed::Box::leak(std::boxed::Box::new(self.0.borrow().buffer.clone()))
    }
}
impl Clone for Entry {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}
impl Widget for Entry {
    fn get_active_state(&self) -> Rc<RefCell<bool>> {
        self.1.clone()
    }
    fn text(&self) -> &'static str {
        std::boxed::Box::leak(std::boxed::Box::new(self.0.borrow().buffer.clone()))
    }
    fn event(&self, event: TEvent, area: Area) -> Request {
        match event {
            TEvent::MouseClick(pos) => {
                if area.contains(&pos) {
                    self.activate(area);
                } else {
                    self.deactivate(area);
                }
            }
            TEvent::Key(Key::Char(c)) if self.is_active() => {
                self.push(c);
                return Request::Redraw;
            }
            TEvent::Key(Key::Backspace) if self.is_active() => {
                self.pop();
                return Request::Redraw;
            }
            TEvent::Key(Key::Enter) if self.is_active() => {
                if let Some(sig) = self.0.borrow().enter_signal.as_ref() {
                    sig(&self);
                    return Request::Redraw;
                }
            }
            _ => (),
        }
        Request::None
    }
}

//****List****
pub struct List(RR<_List>, Rc<RefCell<bool>>);
struct _List {
    items: Vec<Rc<dyn Widget>>,
    filter: Option<&'static str>,
}
impl Widget for List {
    fn get_active_state(&self) -> Rc<RefCell<bool>> {
        self.1.clone()
    }
    fn draw(&self, stdout: &mut std::io::StdoutLock, area: Area) -> Request {
        let children = self.get_children();
        let children_area = self.get_children_area(area);
        let mut request = Request::None;

        queue!(stdout, crossterm::cursor::MoveToColumn(0)).unwrap();
        for (child, area) in children.into_iter().zip(children_area.into_iter()) {
            request.merge(child.draw(stdout, area));
            queue!(stdout, MoveToNextLine(1)).unwrap();
        }
        request
    }
    fn event(&self, event: TEvent, area: Area) -> Request {
        self.propagate_event(event, area)
    }
}
impl Container for List {
    fn add(&self, widget: Rc<dyn Widget>) {
        self.0.borrow_mut().items.push(widget);
    }

    fn clear(&self) {
        self.0.borrow_mut().items.clear();
    }

    fn get_children(&self) -> Vec<Rc<dyn Widget>> {
        if let Some(filter) = self.0.borrow().filter {
            self.get_children_with_filter(filter)
        } else {
            self.0.borrow().items.to_vec()
        }
    }
    fn propagate_event(&self, event: TEvent, area: Area) -> Request {
        let children = self.get_children();
        let children_area = self.get_children_area(area);
        let mut request = Request::None;
        children
            .into_iter()
            .zip(children_area.into_iter())
            .for_each(|(child, area)| request.merge(child.event(event, area)));
        request
    }
    fn get_children_area(&self, area: Area) -> Vec<Area> {
        area.split_vertical(self.get_children_num())
    }
    fn get_children_num(&self) -> usize {
        self.0.borrow().items.iter().count()
    }
}
impl Filterable for List {
    fn get_children_with_filter(&self, filter: &str) -> Vec<Rc<dyn Widget>> {
        self.0
            .borrow()
            .items
            .iter()
            .filter(|child| child.text().contains(filter))
            .cloned()
            .collect()
    }
}
impl Clone for List {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}
impl List {
    pub fn new() -> Rc<Self> {
        Rc::new(Self(
            Rc::new(RefCell::new(_List {
                items: vec![],
                filter: None,
            })),
            Rc::new(RefCell::new(false)),
        ))
    }
    pub fn set_filter(&self, filter: &'static str) {
        self.0.borrow_mut().filter = Some(filter);
    }
    pub fn clear(&self) {
        self.0.borrow_mut().items.clear();
        self.0.borrow_mut().filter = None;
    }
}

//**Grid***
pub struct Grid(RR<_Grid>, Rc<RefCell<bool>>);
struct _Grid {
    items: Vec<Rc<dyn Widget>>,
    width: usize,
    filter: Option<&'static str>,
}
impl Grid {
    pub fn new(width: usize) -> Rc<Self> {
        Rc::new(Self(
            Rc::new(RefCell::new(_Grid {
                items: vec![],
                width,
                filter: None,
            })),
            Rc::new(RefCell::new(false)),
        ))
    }
    pub fn set_filter(&self, filter: &'static str) {
        self.0.borrow_mut().filter = Some(filter);
    }
    fn get_width(&self) -> usize {
        self.0.borrow().width
    }
}

impl Widget for Grid {
    fn get_active_state(&self) -> Rc<RefCell<bool>> {
        self.1.clone()
    }
    fn draw(&self, stdout: &mut std::io::StdoutLock, area: Area) -> Request {
        let children = self.get_children();
        let children_area = self.get_children_area(area);
        let mut request = Request::None;

        queue!(stdout, crossterm::cursor::MoveToColumn(0)).unwrap();
        for (child, area) in children.into_iter().zip(children_area.into_iter()) {
            queue!(
                stdout,
                MoveTo(area.left_up().0 as u16, area.left_up().1 as u16)
            )
            .unwrap();
            request.merge(child.draw(stdout, area));
        }
        request
    }
    fn event(&self, event: TEvent, area: Area) -> Request {
        self.propagate_event(event, area)
    }
}
impl Container for Grid {
    fn add(&self, widget: Rc<dyn Widget>) {
        self.0.borrow_mut().items.push(widget);
    }
    fn clear(&self) {
        self.0.borrow_mut().items.clear();
    }

    fn get_children(&self) -> Vec<Rc<dyn Widget>> {
        if let Some(filter) = self.0.borrow().filter {
            self.get_children_with_filter(filter)
        } else {
            self.0.borrow().items.to_vec()
        }
    }
    fn propagate_event(&self, event: TEvent, area: Area) -> Request {
        let children = self.get_children();
        let children_area = self.get_children_area(area);
        let mut request = Request::None;
        children
            .into_iter()
            .zip(children_area.into_iter())
            .for_each(|(child, area)| {
                request.merge(child.event(event, area));
            });
        request
    }
    fn get_children_area(&self, area: Area) -> Vec<Area> {
        let children_num = self.get_children_num();
        if children_num == 0 {
            return vec![];
        }

        let width = self.get_width();
        let areas = area.split_horizontal(width);
        let height = children_num / width;
        let vertical_areas: Vec<_> = areas
            .into_iter()
            .map(|area| area.split_vertical(height))
            .collect();

        let mut children_area = vec![];
        let mut h_idx = 0;
        let mut v_idx = 0;
        loop {
            children_area.push(vertical_areas[h_idx][v_idx].clone());
            h_idx += 1;
            if h_idx == width {
                h_idx = 0;
                v_idx += 1;
            }
            if v_idx == vertical_areas[0].len() {
                break;
            }
        }
        children_area
    }
    fn get_children_num(&self) -> usize {
        self.0.borrow().items.iter().count()
    }
}
impl Filterable for Grid {
    fn get_children_with_filter(&self, filter: &str) -> Vec<Rc<dyn Widget>> {
        self.0
            .borrow()
            .items
            .iter()
            .filter(|child| child.text().contains(filter))
            .cloned()
            .collect()
    }
}

impl Clone for Grid {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}

//***Events***
#[derive(Clone, Copy)]
pub enum TEvent {
    MouseClick((usize, usize)),
    Nop,
    Key(Key),
}
#[derive(Clone, Copy)]
pub enum Key {
    Char(char),
    Backspace,
    Enter,
}

//***ttk entry point***
pub fn main<Message>(win: &Window, mut receiver: Option<TTkReceiver<Message>>) {
    struct Guard;
    impl Drop for Guard {
        fn drop(&mut self) {
            crossterm::terminal::disable_raw_mode().unwrap();
            crossterm::queue!(std::io::stdout(), LeaveAlternateScreen).unwrap();
            crossterm::queue!(std::io::stdout(), DisableMouseCapture).unwrap();
            crossterm::queue!(std::io::stdout(), Show).unwrap();
        }
    }

    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();
    let _g = Guard;
    crossterm::terminal::enable_raw_mode().unwrap();
    crossterm::queue!(&mut stdout, EnterAlternateScreen).unwrap();
    crossterm::queue!(&mut stdout, EnableMouseCapture).unwrap();
    crossterm::queue!(&mut stdout, Hide).unwrap();
    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || loop {
        tx.send(crossterm::event::read().unwrap()).unwrap();
    });

    let widget = win.get_child(0);
    let mut area = win.get_area();

    // 30fps
    let frame_rate = std::time::Duration::from_micros(33);
    let mut request = Request::Redraw;

    loop {
        //time the main loop
        let now = std::time::Instant::now();

        // check user events
        if let Some(ref mut rx) = receiver {
            if let Ok(e) = rx.rx.try_recv() {
                if let Some(f) = rx.func.as_ref() {
                    (f)(e);
                }
            }
        }
        // check ttk events
        if let Ok(ev) = rx.try_recv() {
            match ev {
                crossterm::event::Event::Mouse(m) => {
                    if let MouseEventKind::Down(_) = m.kind {
                        request.merge(widget.event(
                            TEvent::MouseClick((m.column as usize, m.row as usize)),
                            area.clone(),
                        ));
                    }
                }
                crossterm::event::Event::Key(crossterm::event::KeyEvent {
                    code: crossterm::event::KeyCode::Char(c),
                    modifiers: crossterm::event::KeyModifiers::CONTROL,
                }) => {
                    if c == 'c' {
                        break;
                    }
                }
                crossterm::event::Event::Key(crossterm::event::KeyEvent {
                    code: crossterm::event::KeyCode::Char(c),
                    ..
                }) => {
                    request.merge(widget.event(TEvent::Key(Key::Char(c)), area.clone()));
                }
                crossterm::event::Event::Key(crossterm::event::KeyEvent {
                    code: crossterm::event::KeyCode::Backspace,
                    ..
                }) => {
                    request.merge(widget.event(TEvent::Key(Key::Backspace), area.clone()));
                }
                crossterm::event::Event::Key(crossterm::event::KeyEvent {
                    code: crossterm::event::KeyCode::Enter,
                    ..
                }) => {
                    request.merge(widget.event(TEvent::Key(Key::Enter), area.clone()));
                }
                crossterm::event::Event::Resize(cols, rows) => {
                    area = (cols as usize, rows as usize).into();
                    win.set_area(area.clone());
                    request = Request::Redraw;
                }

                _ => (),
            }
        }

        // draw
        if let Request::Redraw = request {
            queue!(&mut stdout, MoveTo(0, 0)).unwrap();
            queue!(
                &mut stdout,
                crossterm::terminal::Clear(crossterm::terminal::ClearType::All)
            )
            .unwrap();

            //= not merge
            request = widget.draw(&mut stdout, area.clone());
            stdout.flush().unwrap();
        }

        //sync the main loop to the frame rate
        let elapsed = now.elapsed();

        if elapsed < frame_rate {
            std::thread::sleep(frame_rate - elapsed);
        }
    }
}

pub struct TTkReceiver<Message> {
    rx: Receiver<Message>,
    func: Option<std::boxed::Box<dyn Fn(Message)>>,
}
impl<Message> TTkReceiver<Message> {
    pub fn attach(&mut self, func: std::boxed::Box<dyn Fn(Message)>) {
        self.func = Some(func);
    }
}

pub fn channel<Message>() -> (Sender<Message>, TTkReceiver<Message>) {
    let (tx, rx) = mpsc::channel();
    (tx, TTkReceiver { rx, func: None })
}

// helpers
fn div(r: std::ops::Range<usize>, mut d: usize) -> Vec<Range<usize>> {
    if d == 0 || d == 1 {
        return vec![r];
    }
    if d > r.end {
        d = r.end;
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

#[test]
fn test_div() {
    dbg!(div(0..100, 120));
}
