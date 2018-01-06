c = get_config()

c.InteractiveShellApp.extensions = ["autoreload"]
# I'm not often using matplotlib anymore, so disable this for the moment
# c.InteractiveShellApp.exec_lines = ["%autoreload 2", "%matplotlib"]
