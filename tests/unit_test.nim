
import unittest
import os
import osproc
import strutils

import simplePEG
import simplePEG.consts


suite "unit-test suite":
    test "getMessage":
        assert(cHelloWorld == getMessage())
