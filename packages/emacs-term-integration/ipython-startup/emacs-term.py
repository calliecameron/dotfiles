import subprocess
import IPython
import sys
import atexit


def emacs_term_cmd(*args):
    try:
        subprocess.call(args)
    except OSError:
        # Do nothing if the command doesn't exist
        pass


def emacs_term_prompt(self):
    emacs_term_cmd("emacs-dir-tracking")
    emacs_term_cmd("emacs-term-cmd", "term-alert-done")


def emacs_term_preexec():
    emacs_term_cmd("emacs-term-cmd", "term-alert-started")


def emacs_term_shutdown():
    emacs_term_cmd("emacs-term-cmd", "ipython-exit")

IPython.get_ipython().set_hook("pre_prompt_hook", emacs_term_prompt)
IPython.get_ipython().events.register("pre_execute", emacs_term_preexec)
atexit.register(emacs_term_shutdown)

emacs_term_cmd("emacs-term-cmd", "ipython-start",
               str(sys.version_info.major))
