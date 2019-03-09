
CM.make "sources.cm";
SMLofNJ.exportFn (
  "bin/furiganalize",
  fn (n, args) => (Main.run "furiganalize" args; OS.Process.success)
);
