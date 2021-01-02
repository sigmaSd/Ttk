fn play(s: String) {
    std::process::Command::new("pkill")
        .arg("mpv")
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    std::process::Command::new("mpv")
        .arg(&s)
        .stdin(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .spawn()
        .unwrap();
}
fn main() {
    let vbox = ttk::Box::new(ttk::Orientation::Vertical);
    let list = ttk::List::new();
    let list_c = list.clone();

    let label = ttk::Label::new("search: ");
    let entry = ttk::Entry::new();
    let search = ttk::Box::new(ttk::Orientation::Horizontal);
    search.add(label);
    search.add(entry.clone());

    vbox.add(list);
    vbox.add(search);

    let win = ttk::Window::new();
    win.add(vbox);

    entry.connect_enter(move |ent| {
        let client = ureq::agent();

        let req = format!(
            "http://91.132.145.114/json/stations/search?{}",
            ent.get_text()
        );

        let stations: Vec<Station> = client.get(&req).call().into_json_deserialize().unwrap();
        list_c.clear();

        for station in stations {
            let url = station.url.clone();
            let btn = ttk::Button::new(station.name);
            btn.connect_clicked(move || {
                play(url.clone());
            });
            list_c.add(btn);
        }
    });
    ttk::main(&win);
}
#[derive(Debug, serde::Deserialize)]
struct Station {
    name: String,
    url: String,
}
