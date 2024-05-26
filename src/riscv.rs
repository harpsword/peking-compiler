pub(crate) struct RiskVCode {
    insts: Vec<String>,
}

impl RiskVCode {
    pub(crate) fn new() -> RiskVCode {
        RiskVCode { insts: Vec::new() }
    }

    pub(crate) fn append<V: Into<String>>(&mut self, inst: V) {
        self.insts.push(inst.into());
    }

    // fn append_string(&mut self, inst: String) {
    //     self.append(inst);
    // }

    pub(crate) fn append_basic(&mut self) {
        self.append("  .text");
        self.append("  .global main");
    }

    pub(crate) fn generate_result(self) -> String {
        self.insts.join("\n")
    }
}
