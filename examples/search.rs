use ttk::{Box, Entry, Label, List, Orientation, Window};
fn main() {
    let hbox = Box::new(Orientation::Horizontal);
    let label = Label::new("search: ");
    let entry = Entry::new();
    let win = Window::new();
    hbox.add(&label);
    hbox.add(&entry);

    let vbox = Box::new(Orientation::Vertical);
    let list = List::new();

    let l1 = Label::new("hello");
    let l2 = Label::new("world");
    let l3 = Label::new("test");
    let l4 = Label::new("world2");
    list.add(&l1);
    list.add(&l2);
    list.add(&l3);
    list.add(&l4);

    let lc = list.clone();

    entry.connect_changed(move |ent| {
        lc.set_filter(ent.get_text());
    });

    vbox.add(&hbox);
    vbox.add(&list);
    win.add(&vbox);

    ttk::main(&win);
}
