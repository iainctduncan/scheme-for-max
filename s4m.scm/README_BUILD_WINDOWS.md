Notes on attempting to build on windows

- s4m_scm.c is the same file as s4m.scm.c, renamed in a copy for figuring out compiling on windows
 (visual studio doesn't like s4m.scm.c)
- the externals are building ok for me, but on opening them in a max patch (by making an s4m.scm object)
  Max just crashes
- for things work ok, you will need the files scm4max.scm, utilities.scm, and loop.scm in your max search path
- if you install the package (there's a binary on github) that will have everything ready to go, 
  save for the fact that the mxe and mxe64 externals included there aren't working for me.

