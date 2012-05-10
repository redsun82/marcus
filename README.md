
Usage : loremc [OPTIONS] input1 [input2 ...]

Options ('*' marks ones with argument):

* --closure-ready             With 1, try to prevent closure compiler from
                              renaming properties in js parts of the code. With
                              0, disable this feature.
                              Default: 0

* --container, -c             Change the container in which directives will be
                              defined.
                              Default: window

* --default-root, -r          Change the default root variable.
                              Default: root

  --help, -h                  Show this help and do nothing.

* --index-var, -i             Change the position variable.
                              Default: pos

* --lazy-parsing, -l          Enable (1) or disable (0) lazy parsing (stop at
                              first error).
                              Default: 0

* --load-cfg-file, -C         Load a configuration file.

* --old-content-var, -O       Change the variable holding previous contents in
                              DOM assignments.
                              Default: old

* --output, -o                Change the output file name (use %i for input
                              without extension).
                              Default: %i

* --post-process, -e          Add a command to be executed after compilation
                              (use '%c' for compiled file).

* --preamble-append, -p       Append a line to the preamble.

* --preamble-append-file, -P  Append a file to the preamble.

  --preamble-drop             Empty the preamble.

* --preamble-from-file, -F    Load preamble from file, replacing the default.

* --preamble-prepend          Insert a line at the beginning of the preamble.

* --preamble-prepend-file, -B Insert a file at the beginning of the preamble.

* --query-selector, -q        Change the selector macro (use %r for root, %s
                              for selector.
                              Default: %r.querySelectorAll(%s)

  --reset-cfg, -E             Forget all options.

* --selection-mode, -s        Choose the default selection mode.
                              Default: @

* --temp-prefix, -t           Change the prefix of temporary variables.
                              Default: _tmp

* --to-drop-array             Change the name used for the array marking
                              removal.
                              Default: _lorem_to_drop

* --to-drop-field             Change the field name used for marking removal.
                              Default: _lorem_drop

* --weak-parsing, -w          Enable (1) or disable (0) weak parsing, where
                              unrecognized lines are directly passed to
                              compiled code.
                              Default: 0

