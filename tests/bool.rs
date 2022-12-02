use ruler::{enumo::*, s};

#[test]
fn simple() {
    let consts = Workload::Set(vec![s!(true), s!(false)]);
    let vars = Workload::Set(vec![s!(a), s!(b), s!(c)]);
    let uops = Workload::Set(vec![s!(~)]);
    let bops = Workload::Set(vec![s!(&), s!(|), s!(^), s!(->)]);

    let lang = Workload::Set(vec![
        s!(cnst),
        s!(var),
        s!((uop expr expr)),
        s!((bop expr expr)),
    ]);

    let bool3 = lang
        .iter_metric("expr", Metric::Atoms, 3)
        .plug("cnst", consts)
        .plug("var", vars)
        .plug("uop", uops)
        .plug("bop", bops);

    assert_eq!(bool3.force().len(), 130);
}
