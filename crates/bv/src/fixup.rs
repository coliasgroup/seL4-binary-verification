use std::collections::HashSet;

use crate::syntax::{BasicNode, Function, FunctionBody, Node, NodeAddr, NodeId};

impl Function {
    pub(crate) fn fixup(&mut self) {
        if let Some(body) = self.body_mut() {
            body.ensure_simple_entrypoint();
            body.ensure_closed();
        }
    }
}

impl FunctionBody {
    fn fixup(&mut self) {
        self.ensure_simple_entrypoint();
        self.ensure_closed();
    }

    fn ensure_simple_entrypoint(&mut self) {
        let addr = self.fresh_node_addr();
        self.nodes.insert(
            addr,
            Node::Basic(BasicNode {
                next: self.entry_point.clone(),
                var_updates: vec![],
            }),
        );
        self.entry_point = NodeId::Addr(addr);
    }

    fn fresh_node_addr(&self) -> NodeAddr {
        // implementation matches graph_refine.syntax.fresh_node
        let hint = 1;
        let mut n = hint;
        n = (n | 15) + 2;
        while self.nodes.contains_key(&n) {
            n += 16;
        }
        n
    }

    fn ensure_closed(&mut self) {
        let mut dead = HashSet::new();
        for node in self.nodes.values() {
            for cont in node.conts() {
                if let NodeId::Addr(cont_addr) = cont {
                    if !self.nodes.contains_key(&cont_addr) {
                        dead.insert(cont_addr);
                    }
                }
            }
        }
        for addr in dead {
            self.nodes.insert(
                addr,
                Node::Basic(BasicNode {
                    next: NodeId::Err,
                    var_updates: vec![],
                }),
            );
        }
    }
}
