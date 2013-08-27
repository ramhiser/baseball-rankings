# The following script scrapes ESPN's MLB Standings Grid and writes the
# standings for each National League (NL) team to a CSV file, which has the following
# format:
# Team, Opponent, Wins, Losses

from bs4 import BeautifulSoup
import urllib2
import re
import csv

csv_filename = 'cache/NL-standings.csv'

year = '2013'
url = 'http://espn.go.com/mlb/standings/grid/_/year/' + year

page = urllib2.urlopen(url)
soup = BeautifulSoup(page.read())

# Extracts the table for the American League (NL) and the rows for each team
NL_table = soup.find(text = re.compile("National")).find_parent("table")
NL_rows = NL_table.findAll('tr', class_ = re.compile("team"))

# Creates a list of the NL teams and then appends NL for National League
NL_teams = [team_row.find('b').text for team_row in NL_rows]
NL_teams.append("AL")

# Opens a CSV file for the NL standings
with open(csv_filename, 'wb') as f:
    csv_out = csv.writer(f)
    csv_out.writerow(['Team', 'Opponent', 'Wins', 'Losses'])
    
    # For each team in the NL table, identifies the team's name, the opponent,
    # and their wins and losses (WL) against that opponent. Then outputs the
    # results to the open CSV file
    for team_row in NL_rows:
        team = team_row.find('b').text
        
        # A cell has the following form:
        # <td align="right">
        # 7-9</td>
        WL_cells = team_row.findAll('td', align = "right")
        
        # Extracts the values for both wins and losses from each WL table cell
        wins_losses = [td_cell.text.strip('\n').split('-') for td_cell in WL_cells]
        
        # Writes the current team's standings to the CSV file
        for i, opponent in enumerate(NL_teams):
            if team != opponent:
                csv_out.writerow([team, opponent, wins_losses[i][0], wins_losses[i][1]])
