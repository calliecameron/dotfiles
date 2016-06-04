import datetime
import getpass
import platform
import time


def first_line_of_file(filename):
    with open(filename) as f:
        return f.readline().strip()


def file_to_list(filename, test=lambda s: s != ""):
    l = []
    with open(filename) as f:
        for line in f:
            s = line.strip("\n")
            if test(s):
                l.append(s)
    return l


def write_file(s, filename):
    with open(filename, "w") as f:
        f.write(s + "\n")


def write_list_file(l, filename):
    with open(filename, "w") as f:
        for item in l:
            f.write(str(item) + "\n")


def linecount(filename):
    i = 0
    with open(filename) as f:
        for line in f:
            i += 1
    return i


def user_at_host(user, host):
    if user:
        return user + "@" + host
    else:
        return host


def me():
    return user_at_host(getpass.getuser(), platform.node())


def now():
    return datetime.datetime.now().strftime("%Y_%m_%d_%H_%M_%S")


def now_pretty():
    return datetime.datetime.now().strftime("%Y %m %d, %H:%M:%S")


def now_stamp():
    return int(time.time())


def print_and_log(s, *fs):
    print(s)
    log(s, *fs)


def log(s, *fs):
    for f in fs:
        f.write(s + "\n")
        f.flush()
