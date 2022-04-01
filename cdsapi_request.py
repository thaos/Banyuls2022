import cdsapi

c = cdsapi.Client()

c.retrieve(
    'projections-cordex-domains-single-levels',
    {
        'domain': 'europe',
        'experiment': 'historical',
        'horizontal_resolution': '0_11_degree_x_0_11_degree',
        'temporal_resolution': 'daily_mean',
        'variable': [
            '10m_wind_speed', '2m_air_temperature', '2m_surface_specific_humidity',
            'maximum_2m_temperature_in_the_last_24_hours', 'mean_precipitation_flux', 'minimum_2m_temperature_in_the_last_24_hours',
        ],
        'gcm_model': 'ipsl_cm5a_mr',
        'rcm_model': 'ipsl_wrf381p',
        'ensemble_member': 'r1i1p1',
        'start_year': '1951',
        'end_year': '1955',
        'format': 'tgz',
    },
    'download.tar.gz')
