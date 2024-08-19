use std::collections::btree_map::{BTreeMap, Entry};

use regex::Regex;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct ObjdumpInfo {
    pub(crate) symbols: BTreeMap<String, Symbol>,
    pub(crate) sections: BTreeMap<String, Section>,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct Symbol {
    pub(crate) addr: u64,
    pub(crate) size: u64,
    pub(crate) section: String,
}

impl Symbol {
    fn end(&self) -> u64 {
        self.addr + self.size
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub(crate) struct Section {
    pub(crate) addr: u64,
    pub(crate) size: u64,
}

impl Section {
    fn end(&self) -> u64 {
        self.addr + self.size
    }
}

impl ObjdumpInfo {
    pub(crate) fn parse_from_str(s: &str) -> Self {
        let re = Regex::new(
            r"(?x)
            ^
                (?<addr>.+?)
                \s+
                .+?
                \s+
                .+?
                \s+
                (?<section>.+?)
                \s+
                (?<size>.+?)
                \s+
                (?<name>.+?)
            $
        ",
        )
        .unwrap();

        let mut lines = s.lines().filter(|line| !line.is_empty());

        while lines.next().unwrap() != "SYMBOL TABLE:" {}

        let mut symbols = BTreeMap::new();
        for line in lines {
            let caps = re.captures(line).unwrap();
            symbols.insert(
                caps["name"].to_owned(),
                Symbol {
                    addr: u64::from_str_radix(&caps["addr"], 16).unwrap(),
                    size: u64::from_str_radix(&caps["size"], 16).unwrap(),
                    section: caps["section"].to_owned(),
                },
            );
        }

        let mut sections = BTreeMap::new();
        for symbol in symbols.values() {
            match sections.entry(symbol.section.to_owned()) {
                Entry::Vacant(entry) => {
                    entry.insert(Section {
                        addr: symbol.addr,
                        size: symbol.size,
                    });
                }
                Entry::Occupied(mut entry) => {
                    let section = entry.get_mut();
                    let end = section.end().max(symbol.end());
                    section.addr = section.addr.min(symbol.addr);
                    section.size = end - section.addr;
                }
            }
        }

        Self { symbols, sections }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::tests::utils::*;

    use super::*;

    #[test]
    fn parse() {
        let mut paths = vec![];
        for opt_level in ["O1", "O2"] {
            paths.push(
                graph_refine_dir()
                    .join("loop-example")
                    .join(opt_level)
                    .join(format!("loop-{opt_level}.elf.symtab")),
            );
        }
        paths.push(sel4_target_dir().join("kernel.elf.symtab"));

        for path in paths {
            log::trace!("testing parse: {}", path.display());
            ObjdumpInfo::parse_from_str(&fs::read_to_string(path).unwrap());
        }
    }
}
