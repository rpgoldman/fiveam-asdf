# FIVEAM-ASDF : Integrating System Definitions and FiveAM testing #

FIVEAM-ASDF defines a new system class `FIVEAM-TESTER` that provides functionality for running tests using FiveAM and raising an error if the tests fail, or if an unexpected number of tests run.  This provides a definition for the `ASDF:TEST-OP` operation of the defined system.  It's useful for incorporation into a continuous integration framework, as well.

How to use
----------

The "fiveam-asdf" ASDF system adds a new subclass of `ASDF:SYSTEM`.  If you wish to test a system,  `foo`, it is usually best to create a subsystem like this:

    (defsystem foo
      :in-order-to ((test-op (test-op "foo/test")))
      ...)

    (defsystem "foo/test"
      :defsystem-depends-on ("fiveam-asdf")
      :depends-on ("fiveam" "foo" ...)
      :class fiveam-tester
      ....)

Having the tests in this ancillary subsystem avoids the need for your main system to depend on FiveAM itself, so it (and your test definitions) will only be loaded if you actually want to run the tests.

Now, add files with FiveAM tests in them into the components of `foo/test`, as you wish.  See the FiveAM docs for how to do this.

### Integrating with ASDF's TEST-OP ###

List the names of all of your tests in the `:TEST-NAMES` property of your test system's defsystem.  Entries in here can either be a symbol designator or a cons whose `CAR` is a symbol designator and whose `CDR` is a package designator.

If all of your FiveAM test names are defined in a single package, you can set the `:test-package` property of your `FIVEAM-TESTER` system, and any package *un*qualified elements of `:test-names` will be searched for in the `:test-package`.

Note that FiveAM offers named test *suites*.  Typically it will be most convenient to make a small number of such suites for your system, and name only these suites in `test-names`, rather than naming each individual test.

Doing `(asdf:test-system "foo/test")` will run all the tests in the `fiveam-tester` system.  However, as above, it is best to use ASDF's `:in-order-to` to connect `foo` to `foo/test` for testing purposes.  Then a user can simply do `(asdf:test-system "foo")` without needing to know how the tests are implemented.

If any tests fail, a condition of type `asdf:fiveam-asdf-failure` will be raised. It is necessary to do this, because `asdf:test-system` does not return a value.  One can easily write a test script that will run `asdf:test-system` and either exit with 0 if the tests all pass, or 1 otherwise.

### Test counts ###

In our experience, it was possible to introduce bugs that would cause tests not to be run at all, which could appear indistinguishable from all the tests passing, since all the tests *that were run* would pass.

So we added the `:num-checks` option to `fiveam-tester` systems.  If you specify a number of checks (note that in FiveAM the number of checks is *not*, in general, the same as the number of *tests*), then a condition of type `fiveam-wrong-number-of-checks` will be raised if a differernt number of checks are actually run.

Bugs, misfeatures, etc.
-------------------

This extension mistakenly lives directly in the `ASDF` Common Lisp package. It should not, but I have not had the time to fix it.
