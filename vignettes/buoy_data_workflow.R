possible.stations = get_closest_buoys(lat = 41.43, lon = -71.46, max_dist = 50, n = 3)

this_station = possible.stations$station_id[which.min(possible.stations$distance_km)]

get_station_metadata(station_id = this_station)

buoy.dat = pull_buoy_data(station_id = this_station,start_date = '2026-01-01',end_date = '2026-12-31',var_name = c('WTMP','WSPD'))

plot_buoy_data(station_id = this_station,
               start_date = '2026-01-01',
               end_date = '2026-12-31',
               var_name = c('WTMP','WSPD'),
               agg_time = 'day',
               write = T,
               file_name = here::here('test_plot.png'))
