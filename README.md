omd
===

Short description
-----------------

a (hopefully efficient) Markdown frontend in "simple OCaml" (i.e., no external dependencies)

This implementation is meant to be more Github Markdown (the one used in Github) than original Markdown (<http://daringfireball.net/projects/markdown/syntax>).

----------------

Usage
-----

- to get the latest version of omd

        git clone git@github.com:pw374/omd.git

- or if you don't have git installed but have svn

        svn co https://github.com/pw374/omd

- to compile omd
  - using oasis

          cd omd && make

  - without oasis

          cd omd/src && make

- to use omd

      ./omd -help

- to install omd, for the moment, you may do this:

      cd omd/src && make omd && cp omd /path/to/where/you/want/to/install/omd

----------------

N.B. You probably need to know markdown first if you plan on reading
the parser's source code. This implementation was driven mainly by two
things: the original Markdown syntax documentation
(<http://daringfireball.net/projects/markdown/syntax>) and
try-and-see-what-it-produces on
<https://github.com/pw374/sandbox>. Also, I used Pandoc to check its
behaviour sometimes. Now I can say than Pandoc does not really support
Github flavour Markdown even if it pretends to do so.

