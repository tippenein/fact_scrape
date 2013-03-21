'''
file: util.py

'''
import datetime

def create_datetime(date):
    ''' Takes a list ['2013', 'feb', '14'] and returns a datetime object
    '''
    y = int(date[0])
    m = month_to_int(date[1])
    d = int(date[2])
    return datetime.date(y, m, d)

def month_to_int(month):
    ms = ['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
    return ms.index(month)+1
