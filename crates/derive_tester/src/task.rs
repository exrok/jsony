use std::cell::Cell;
use std::collections::HashMap;

use jsony::TextWriter;
use proc_macro2::{Ident, Literal, TokenTree};
use rand::prelude::*;

use crate::bench::{Benchy, BuildProfile, Perf, Scenario};
use crate::datagen::Rand;
use crate::library::Libary;
use crate::schema::{Field, ItemKind, Struct, Type, Types, IMPL_JSON};

pub enum CodeLayout {
    SingleFile,
    SeparateModule,
    SeparateCrate,
}

pub struct LotsOfStructs<'a> {
    pub structs: Vec<&'a Struct<'a>>,
    pub json_input: HashMap<String, String>,
}

impl<'a> LotsOfStructs<'a> {
    pub fn new(amount: u64, types: &'a mut Types<'a>) -> LotsOfStructs<'a> {
        let mut rand = Rand {
            rng: StdRng::from_seed([0u8; 32]),
            steam: 95,
        };
        let mut structs = Vec::new();
        let mut json_input: HashMap<String, String> = HashMap::new();
        for struct_index in 0..amount {
            let mut fields = Vec::new();
            for i in 0..rand.rng.gen_range(1..10) {
                fields.push(Field {
                    name: Ident::owned(format!("f{i}")),
                    inner_type: Cell::new(**types.db.choose(&mut rand.rng).unwrap()),
                    rename: None,
                    default: false,
                });
            }
            let rand_struct = types.create_struct(
                ItemKind::Struct,
                Ident::owned(format!("S{struct_index}")),
                fields,
                rand.rng.gen(),
                IMPL_JSON,
            );
            let mut output = TextWriter::new();
            rand_struct.random_json(&mut rand, &mut output);
            json_input.insert(rand_struct.name.to_string(), output.into_string());

            structs.push(rand_struct);
            if rand_struct.stack_depth < 4 {
                types.insert(Type::Struct(rand_struct));
            }
        }

        LotsOfStructs {
            structs,
            json_input,
        }
    }

    pub fn codegen_models(&self, lib: &Libary) -> Vec<u8> {
        let mut out: Vec<TokenTree> = Vec::new();
        lib.gen_prelude(&mut out);
        for a_struct in &self.structs {
            a_struct.generate_def(&mut out, lib);
        }
        crate::token::to_rust(out.into_iter().collect())
    }

    pub fn codegen(&self, lib: Libary) -> Vec<TokenTree> {
        let mut rust_code: Vec<TokenTree> = Vec::new();
        lib.gen_prelude(&mut rust_code);
        for a_struct in &self.structs {
            a_struct.generate_def(&mut rust_code, &lib);
        }
        let count = self.structs.len();
        splat!((&mut rust_code); fn main() {
            let mut arg = std::env::args();
            arg.next();
            let ty = arg.next().unwrap();
            let repeat: i64 = arg.next().unwrap().parse().unwrap();
            let output = arg.next().is_some();

            let mut input = String::new();
            use std::io::Read;
            std::io::stdin().read_to_string(&mut input).unwrap();
            for _ in [#Literal::usize_unsuffixed(0)]..repeat {
                std::hint::black_box(&mut input);
                let text = match ty.as_str() {
                    [for (struct_index in 0..count) {
                    [@Literal::string(&format!("S{struct_index}"))] => {
                        let value = [lib.gen_from_json_str(
                            &mut rust_code,
                            &Ident::owned(format!("S{struct_index}")),
                            sfn!(out; &input)
                        )].unwrap();
                        [lib.gen_to_json_string(&mut rust_code, sfn!(out; &value))]
                    }}]
                    _ => {
                        panic!("Unrecognized format: {:?}", ty);
                    }
                };
                if output {
                    println!("{}", text.len())
                }
            }
        });
        rust_code
    }

    pub fn codegen_adv(&self, lib: Libary) -> Vec<TokenTree> {
        let mut rust_code: Vec<TokenTree> = Vec::new();
        lib.gen_prelude(&mut rust_code);
        for a_struct in &self.structs {
            a_struct.generate_adv_def_jsony(&mut rust_code);
        }
        let count = self.structs.len();
        splat!((&mut rust_code); fn main() {
            let mut arg = std::env::args();
            arg.next();
            let ty = arg.next().unwrap();
            let repeat: i64 = arg.next().unwrap().parse().unwrap();
            let output = arg.next().is_some();

            let mut input = String::new();
            use std::io::Read;
            std::io::stdin().read_to_string(&mut input).unwrap();
            for _ in [#Literal::usize_unsuffixed(0)]..repeat {
                std::hint::black_box(&mut input);
                let text = match ty.as_str() {
                    [for (struct_index in 0..count) {
                    [@Literal::string(&format!("S{struct_index}"))] => {
                        let value = [lib.gen_from_json_str(
                            &mut rust_code,
                            &Ident::owned(format!("S{struct_index}")),
                            sfn!(out; &input)
                        )].unwrap();
                        [lib.gen_to_json_string(&mut rust_code, sfn!(out; &value))]
                    }}]
                    _ => {
                        panic!("Unrecognized format: {:?}", ty);
                    }
                };
                if output {
                    println!("{}", text.len())
                }
            }
        });
        rust_code
    }

    pub fn setup_bench(&self, name: &str, lib: Libary) -> anyhow::Result<Benchy> {
        let b = Benchy::open(name, &lib.dependencies())?;
        b.write("main.rs", self.codegen(lib))?;
        Ok(b)
    }

    pub fn bench(
        &self,
        name: &str,
        lib: Libary,
        scenarios: &[Scenario],
        samples: u32,
    ) -> anyhow::Result<(Vec<(String, String)>, Vec<(Scenario, Perf)>)> {
        let mut b = self.setup_bench(name, lib.clone())?;
        let mut result = Vec::new();
        for scenario in scenarios {
            let perf = if let Scenario::RuntimeBenchmark { profile } = scenario {
                if let Libary::Merde = lib {
                    if let BuildProfile::Debug = profile {
                        continue;
                    }
                }
                b.perf_run_time(
                    *profile,
                    "S40 100000".into(),
                    self.json_input["S40"].as_bytes(),
                )
            } else {
                b.bench(samples, scenario.clone())
            };
            let perf = perf?;
            println!("{:?}\n   {}", scenario, perf);
            result.push((scenario.clone(), perf));
        }
        let versions = b.read_versions();
        Ok((versions, result))
    }
}
