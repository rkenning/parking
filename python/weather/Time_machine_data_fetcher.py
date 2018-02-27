import requests, csv
import datetime as dt, calendar
from calendar import monthrange
from time import sleep
from secrets import API_KEY

BASE_URL = f"https://api.darksky.net/forecast/{API_KEY}/"
QUERY_PARAMS = "?units=si"

BATH_LAT = 51.3758
BATH_LONG = 2.3599
BATH_URL = f"{BASE_URL}{BATH_LAT},{BATH_LONG}{QUERY_PARAMS}"

class DataFetcher:
    """
    The DataFetcher uses the darksky.net API to fetch historical and predicted
    weather forecasts.
    """

    def __init__(self):
        # Requirement for using the API.
        # Should we ever make a more GUI friendly application that takes
        # advantage of this API we'll need to put this as near the data
        # as possible.
        print("Powered by Dark Sky")

    #def persist_current_weather_data(self):
    #    payload_dict = requests.get(BATH_URL).json()
    #    self.__persist_to_mongo(payload_dict)

    def get_timemachine(self,TIME):
        BATH_URL_HIST = f"{BASE_URL}{BATH_LAT},{BATH_LONG},{TIME}{QUERY_PARAMS}"
        print(BATH_URL_HIST)
        payload_dict = requests.get(BATH_URL_HIST).json()
        return payload_dict

    def get_relevant_data(self, payload_dict):

        desired_data = [None] * len(payload_dict['hourly']['data'])
        # relevant_keys = ('time',
        #                  'temperature',
        #                  'apparentTemperature',
        #                  'percipIntensity',
        #                  'precipProbability',
        #                  'humidity',
        #                  'cloudCover'
        #                  'windSpeed',
        #                  'windGust',
        #                  'visibility',
        #                  'summary'
        #                   )

        for i in range(len(payload_dict['hourly']['data'])): # RK Change from fixed 24 value as some
                # hourly data does not have 24 hours
            #hourly_data = payload_dict['hourly']['data'][i]
            desired_data[i] = payload_dict['hourly']['data'][i]
        return desired_data

    def add_formatted_date(self, desired_data):
        length = len(desired_data)
        desired_data_date = [None] * len(desired_data)
        for i in range(len(desired_data)):
            if desired_data[i] is not None:
                current_dic = desired_data[i]
                unix_time = current_dic['time']

                #format the unix time to more friendly
                formatted_date = dt.datetime.utcfromtimestamp(
                    int(unix_time)
                    ).strftime('%Y-%m-%d %H:%M:%S')

                # Add the formatted date to the object
                desired_data[i]['FormattedDate']= formatted_date
        return desired_data


 #Default Calender Get dates method includes additional dates to make a full week so ended up writing a custom function
def get_datetime_range(year, month):
    nb_days = monthrange(year, month)[1]
    return [dt.date(year, month, day) for day in range(1, nb_days+1)]


def process_timemachine_Month(Year, Month):
    combined_list=[]
    cal = calendar.Calendar()
    for x in get_datetime_range(Year, Month):
        date_string = '{:%Y-%m-%dT%H:%M:%S}'.format(x)
        #if date_string == '2017-10-29T00:00:00':
        if 1==1: #Debug if required
            print(f"Requesting Date:{date_string}")
            fetcher = DataFetcher().get_timemachine(date_string)
            relevant_data = DataFetcher().get_relevant_data(fetcher)
            relevant_data = DataFetcher().add_formatted_date(relevant_data)
            combined_list += relevant_data
            #break # testing

    return combined_list


def export_dict_list_to_csv(data, filename):
    with open(filename, 'w', ) as f:
        # Assuming that all dictionaries in the list have the same keys.
        #print(data[0])
        headers = sorted([k for k, v in data[0].items()])
        #headers = sorted([k for k, v in data[0]].items())
        csv_data = [headers]

        for d in data:
            tempdict = {}
            for h in headers:
                if d is not None:
                    if h in d :
                        #Some of the 'standard' keys are missing from the objects sometimes so skip them if they are
                        tempdict[h]=d[h]
                    else:
                        tempdict[h] = ''

            csv_data.append(tempdict.values())

        writer = csv.writer(f,lineterminator='\n', delimiter=',')
        writer.writerows(csv_data)

def main():
    year = 2013 #Define the required year to process
    #Itterate though the month numbers for the year
    for month in range(12,13):
        combined_list = []
        timemachine_payload = process_timemachine_Month(year, month)
        combined_list+= timemachine_payload
        filename = f"./Output/DarkSkyTimeMachine-{year}-{month}.csv"
        print(f"Writing file :{filename}")
        export_dict_list_to_csv(combined_list,filename)
        sleep(5)

if __name__ == "__main__":
    main()