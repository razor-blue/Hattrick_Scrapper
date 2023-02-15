/*import scrapy

class PlayerScraper(scrapy.Spider):
name = "player_scraper"
start_urls = [
'https://www.hattrick.org/pl/Club/Players/player.aspx?PlayerId=474384214',
]

def parse(self, response):
playing_field = response.css('#your_playing_field_selector ::text').get()
yield {
  'playing_field': playing_field
}*/
