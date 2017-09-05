
import unittest
import os
import osproc
import strutils

import simplePEG
import simplePEG.consts

include "../scripts/nim/scriptsEnvVarNames.nimInc"

suite "integration-test suite":
  test "getMessage excecuting the app":
    assert(cHelloWorld == execProcess(getEnv(gcApplicationToTestEnvVarName)).strip())
