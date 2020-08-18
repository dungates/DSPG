
import matplotlib # sudo python3 -m pip install matplotlib
import matplotlib.pyplot as plt
from climata.usgs import DailyValueIO
from climata.usgs import InstantValueIO
import pandas as pd # sudo python3 -m pip install pandas
from pandas.plotting import register_matplotlib_converters
import numpy as np # sudo python3 -m pip install numpy

register_matplotlib_converters()
plt.style.use('ggplot')
plt.rcParams['figure.figsize'] = (20.0, 10.0)


# set parameters
nyears = 15
ndays = 365 * nyears

station_id = "14092500"
param_id = "00010"

datelist = pd.date_range(end = pd.datetime.today(), periods = ndays).tolist()

ivals = InstantValueIO(station = station_id, parameter = param_id, start_date = "2019-08-18", end_date = "2020-08-18")
for ival in ivals:
    print
    print(ival.site_code, ival.variable_code)
    for row in ival.data:
        print(row.date, row.value)


# data = DailyValueIO(
#     start_date = datelist[0],
#     end_date = datelist[-1],
#     station = station_id,
#     parameter = param_id,
# )
# 
# 
# # create lists of date-flow values
# for series in data:
#     flow = [r[1] for r in series.data]
#     dates = [r[0] for r in series.data]
#     
# plt.plot(dates, flow)
# plt.xlabel('Date')
# plt.ylabel('Temperature (Celsius °)')
# plt.title(series.site_name)
# plt.xticks(rotation = 'vertical')
# plt.show()
# 
# print(flow[-1])
# dates[-1]














