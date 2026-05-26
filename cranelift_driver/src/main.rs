use std::collections::HashMap;
use std::env;
use std::fs;
use cranelift_codegen::Context;
use cranelift_codegen::ir::{ExternalName, UserExternalName, UserFuncName};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_codegen::isa;
use cranelift_module::{Module, Linkage, FuncId};
use cranelift_object::{ObjectBuilder, ObjectModule};
use cranelift_reader::parse_functions;
use target_lexicon::Triple;

fn clean(name: &UserFuncName) -> String {
    match name {
        UserFuncName::Testcase(tc) => tc.to_string().trim_start_matches('%').to_string(),
        UserFuncName::User(u) => format!("u{}_{}", u.namespace, u.index),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("usage: orto-clif <input.clif> <output.o>");
        std::process::exit(2);
    }
    let src = fs::read_to_string(&args[1]).expect("read input");
    let funcs = parse_functions(&src).expect("parse clif");

    let mut fb = settings::builder();
    fb.set("opt_level", "speed").unwrap();
    let flags = settings::Flags::new(fb);
    let isa = isa::lookup(Triple::host()).expect("isa").finish(flags).expect("isa finish");
    let builder = ObjectBuilder::new(isa, "orto", cranelift_module::default_libcall_names()).unwrap();
    let mut module = ObjectModule::new(builder);

    // pass 1: declare every defined function so cross-references resolve
    let mut defined: HashMap<String, FuncId> = HashMap::new();
    let mut pending = Vec::new();
    for func in funcs {
        let name = clean(&func.name);
        let id = module
            .declare_function(&name, Linkage::Export, &func.signature)
            .expect("declare defined");
        defined.insert(name, id);
        pending.push((func, id));
    }

    // pass 2: remap external (TestcaseName) refs to module FuncIds, then define
    for (mut func, id) in pending {
        let frs: Vec<_> = func.dfg.ext_funcs.keys().collect();
        for fr in frs {
            let data = func.dfg.ext_funcs[fr].clone();
            if let ExternalName::TestCase(tc) = &data.name {
                let sym = tc.to_string().trim_start_matches('%').to_string();
                let target = *defined.entry(sym.clone()).or_insert_with(|| {
                    let sig = func.dfg.signatures[data.signature].clone();
                    module
                        .declare_function(&sym, Linkage::Import, &sig)
                        .expect("declare import")
                });
                let uref = func
                    .params
                    .ensure_user_func_name(UserExternalName::new(0, target.as_u32()));
                func.dfg.ext_funcs[fr].name = ExternalName::user(uref);
            }
        }
        func.name = UserFuncName::user(0, id.as_u32());
        let mut ctx = Context::for_function(func);
        module.define_function(id, &mut ctx).expect("define");
    }

    let product = module.finish();
    let bytes = product.emit().expect("emit");
    fs::write(&args[2], bytes).expect("write .o");
    eprintln!("wrote {}", args[2]);
}
