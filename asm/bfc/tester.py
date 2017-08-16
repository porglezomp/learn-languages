from __future__ import print_function

from subprocess import Popen, PIPE
import tempfile
import os

CASES = [
    ('', []),
    ('.>' * 10, [0] * 10),
    ('.', [0]),
    ('+.', [1]),
    ('++>++<[>+<-]>.', [4]),
    ('++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++.'
     '.+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.',
     [ord(c) for c in "Hello World!\n"]),
]

MSG = """\
Test case: {case}
Exit     : {status}
Expected : {expected}
Got      : {got}
"""

errors = []

for in_buf, out_buf in CASES:
    exefile, exename = tempfile.mkstemp()
    try:
        proc = Popen(['./BFA'], stdin=PIPE, stdout=exefile)
        proc.communicate(in_buf)
        os.close(exefile)
        proc = Popen([exename], stdout=PIPE, stderr=PIPE)
        out_result, err_result = proc.communicate()
        out_result = [ord(c) for c in out_result]
        if out_buf != out_result or proc.returncode != 0:
            print('E', end='')
            errors.append(MSG.format(
                case=in_buf,
                status=proc.returncode,
                expected=out_buf,
                got=out_result,
            ))
            errors.append([ord(c) for c in err_result])
        else:
            print(".", end='')
    finally:
        os.remove(exename)

print()

for error in errors:
    print(error)
