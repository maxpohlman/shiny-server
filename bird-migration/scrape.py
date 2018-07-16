from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys
import time
import numpy as np
import pandas as pd
import asyncio
from selenium.webdriver.support import expected_conditions as EC
import logging

#%%
df = pd.read_csv("C:/Users/mpohlman/Documents/bird/bird_names_two.csv")
#%%
bird = 'Great Blue Heron' #Mallard
birds = ['Dendrocygna autumnalis','Dendrocygna bicolor','Nomonyx dominicus','Oxyura jamaicensis','Fake bird: iwsufhsif','Cygnus buccinator']

smbirds = ['Dendrocygna bicolor', 'Fake bird: iwsufhsif', 'Nomonyx dominicus']



chrome_options = webdriver.ChromeOptions()
prefs = {'download.default_directory' : 'C:/Users/mpohlman/Documents/bird/data/zips'}
chrome_options.add_experimental_option('prefs', prefs)
driver = webdriver.Chrome('C:/Users/mpohlman/Documents/bird/chromedriver.exe',chrome_options=chrome_options)



url = 'https://www.inaturalist.org/observations/export'
#%%
def get_data(bird):

    driver.get(url)

    driver.find_element_by_id('research_grade_checkbox').click()


    driver.find_elements_by_link_text('None')[0].click()
    driver.find_elements_by_link_text('None')[1].click()
    driver.find_elements_by_link_text('None')[2].click()

    driver.find_element_by_id('observations_export_flow_task_options_columns_image_url').click()
    driver.find_element_by_id('observations_export_flow_task_options_columns_place_guess').click()
    driver.find_element_by_id('observations_export_flow_task_options_columns_latitude').click()
    driver.find_element_by_id('observations_export_flow_task_options_columns_longitude').click()
    driver.find_element_by_id('observations_export_flow_task_options_columns_scientific_name').click()
    driver.find_element_by_id('observations_export_flow_task_options_columns_common_name').click()
    driver.find_element_by_id('observations_export_flow_task_options_columns_time_observed_at').click()
    driver.find_element_by_id('observations_export_flow_task_options_columns_time_zone').click()




    entry = driver.find_element_by_id('obs_filter_taxon_name')
    entry.send_keys(bird)

    wait = WebDriverWait(driver, 1000000)
    element = wait.until(EC.presence_of_element_located((By.ID, 'ui-id-2')))

    driver.implicitly_wait(2)

    tst = element.find_element_by_tag_name('li')
    tst.click()

    try:
        wait = WebDriverWait(driver, 1)
        element = wait.until(EC.presence_of_element_located((By.ID, 'fake_id_that_doesnt_exist')))
    except:
        pass


    query = driver.find_element_by_id('query')
    query.submit()


    wait = WebDriverWait(driver, 48000)
    element = wait.until(EC.presence_of_element_located((By.ID, 'success')))
    element.find_element_by_tag_name('table').find_element_by_tag_name('tbody').find_element_by_tag_name('tr').find_element_by_tag_name('td').find_element_by_tag_name('a').click()

#%%

succ = []
fail = []

logging.basicConfig(filename = "C:/Users/mpohlman/Documents/bird/scraping.log", level = logging.DEBUG)

#TODO: Add iteration number
for bird in fail[2:]:
    try:
        get_data(bird)
        print('Successfully downloaded bird: {}'.format(bird))
        logging.warning('Successfully downloaded bird: {}'.format(bird))
        succ.append(bird)
    except:
        print('Error downloading bird: {}'.format(bird))
        logging.warning('Error downloading bird: {}'.format(bird))
        fail.append(bird)
        try:
            alert = driver.switch_to.alert
            alert.accept()
        except:
            pass
