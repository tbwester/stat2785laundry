#!/usr/bin/env python

import numpy as np
import pp
import datetime as dt
import os, sys, time

def astime(thedate):
    return dt.datetime.strptime(thedate, '%Y-%m-%d %H:%M:%S')

def diff_dates(date1, date2):
    return float(abs(date2-date1).total_seconds() / 60.)

def file_len(fname):
    with open(fname) as f:
        for i, l in enumerate(f):
            pass
    return i + 1

def coincidence(fname):
    """Get list of machines in use when any given machine starts"""
    ifile = 'data/' + fname
    if os.stat(ifile).st_size == 0:
        return []
    if file_len(ifile) < 3:
        return []

    data = np.genfromtxt(ifile, dtype=None,
                         names = ['start_time','end_time','extend_time',
                         'idle_time','num','kind','dorm'], delimiter=",")

    #Sort data by end time
    data = np.sort( data, order=['start_time'] )

    longname = data[0]['dorm']

    n_inuse = []
    list_inuse = []
    for i in range(0, len(data)):
    #for i in range(0, 100):
        n_available = 0
        coin_list = ""

        for j in range(0, len(data)):
            ## Don't check for coincidence for times after
            if astime(data[j][0]) >= astime(data[i][0]):
                break # this is ok since ordered

            ## Don't check for coincidence with runs > 2hrs before this one
            if astime(data[j][0]) + dt.timedelta(hours=2) < astime(data[i][0]):
                continue

            ## Don't check for coincidence from the same machine
            if data[j][4] == data[i][4]:
                continue

            ## Don't check coincidence for other type of machine
            if data[j][5] != data[i][5]:
                continue

            totaltimej = diff_dates(astime(data[j][0]), astime(data[j][1])) + data[j][2] + data[j][3]

            ## Actually check to see if the machine is available at the time
            if astime(data[j][0]) + dt.timedelta(minutes=totaltimej) > astime(data[i][0]):
                #print data[i][1]

                n_available += 1
                if coin_list != "":
                    coin_list += ',' + str(data[j][4])
                else:
                    coin_list += str(data[j][4])

        n_inuse.append(n_available)
        list_inuse.append(coin_list)

    return list_inuse

if __name__=="__main__":

    ppservers = ()
    if len(sys.argv) > 1:
        ncpus = int(sys.argv[1])
        # Creates jobserver with ncpus workers
        job_server = pp.Server(ncpus, ppservers=ppservers)
    else:
        # Creates jobserver with automatically detected number of workers
        job_server = pp.Server(ppservers=ppservers)

    print "Starting pp with", job_server.get_ncpus(), "workers"

    start_time = time.time()

    inputs = []
    for fn in os.listdir("data/"):
        inputs.append(fn)
    print "Inputs are", inputs
    jobs = [(input, job_server.submit(coincidence, (input,), (astime,diff_dates,file_len,), ("numpy as np","datetime as dt",))) for input in inputs]
    for input, job in jobs:
        print "List for", input, "is", job()
        with open(input.split('.')[0] + "_coin.txt", "w") as f:
            stroutput = ''
            for i in range(0,len(job())):
                stroutput += job()[i] + "\n"
            f.write(stroutput)

    print "Time elapsed: ", time.time() - start_time, "s"
    job_server.print_stats()
