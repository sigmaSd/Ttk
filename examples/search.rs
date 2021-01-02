use ttk::{Box, Container, Entry, Label, List, Orientation, Window};
fn main() {
    let hbox = Box::new(Orientation::Horizontal);
    let label = Label::new("search: ");
    let entry = Entry::new();
    let win = Window::new();
    hbox.add(label);
    hbox.add(entry.clone());

    let vbox = Box::new(Orientation::Vertical);
    let list = List::new();

    list.add(Label::new("hello1"));
    list.add(Label::new("hello2"));
    list.add(Label::new("world"));
    list.add(Label::new("test"));

    let lc = list.clone();

    entry.connect_changed(move |ent| {
        lc.set_filter(ent.get_text());
    });

    vbox.add(hbox);
    vbox.add(list);
    win.add(vbox);

    ttk::main(&win);
}
