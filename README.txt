# Project Title: MELBOURNE CITY EXPLORATION

## Project Description: This project creates an interactive map interface to help visitors explore food and attractions in the city of Melbourne. It is designed to provide an easy and engaging way for tourists to discover the best dining and entertainment options available in the area.

## Feature:
Two dashboards (City Dining and City Attractions) are built in Tableau
Each of dashboards including:
- Dashboard Switch: Two buttons (City Dining and City Attraction) to switch between dashboards.
- Static Categories: Display total of numbers for different categories in Food/Attraction Places.
- Interactive Map (R codes has been integrated Tableau): User-friendly map to view locations
	- Filter Options: Narrow choices by restaurant or attraction type.
	- Top 20 Food/Attraction Places to Go/Try: List of must-try restaurants and must-go attractions
	- Transportation Checkboxs:
		- City Tram Route: A special tram route in Melbourne.
 		- All Tram Stations: View all tram stops in the City of Melbourne.
	- Search Functionality: Quickly find specific restaurants or attractions.
	- Zoom In: Click on a specific marker to display nearby food places within a 200-meter radius and attractions within a 500-meter radius.
	- Zoom Out: Use the Zoom Out button to see an overview of the City of Melbourne map.

## Source: From the City of Melbourne
	- City Dining: https://data.melbourne.vic.gov.au/explore/dataset/cafes-and-restaurants-with-seating-capacity/information/
	- City Attraction: https://data.melbourne.vic.gov.au/explore/dataset/landmarks-and-places-of-interest-including-schools-theatres-health-services-spor/information

## Target Audience
This project aims at tourists and visitors planning to explore Melbourne, which provides them with a comprehensive guide to enhance their travel experience.

## Getting Started with the Project
This project includes two R projects and one Tableau file, all of which are integrated. To view the dashboard, you will need to run both R projects in parallel and then open the Tableau file. Follow these steps:

### Prerequisites
- Install R and RStudio (if not already installed):
- Install Tableau (if not already installed):
- Download Tableau from Tableau website.

### Step to Get Started
Step 1: Open and run 'City Dining' Project
- In the 'GEOM90007_Assignment3_Group7' folder, open the 'City Dining.Rproj' (R Project)
- Click on File > Open File...
- Navigate to the 'GEOM90007_Assignment3_Group7' folder
- Select the 'City Dining.R' (R File) and click Open
- Once the script is loaded, run the script and keep it open

Step 2: Open and run 'City Attractions' Project
- In the 'GEOM90007_Assignment3_Group7' folder, open the 'City Attractions.Rproj' (R Project)
- Click on File > Open File...
- Navigate to the 'GEOM90007_Assignment3_Group7' folder
- Select the 'City Attractions.R' (R File) and click Open
- Once the script is loaded, run the script and keep it open

Step 3: Open Tableau file
- In the 'GEOM90007_Assignment3_Group7' folder, open the 'Interface platform.twb' (Tableau Workbook)
- The automatic notification will request to reconnect the dashboard with their dataset 
- Click on "Edit Connection" button
- Navigate to the 'GEOM90007_Assignment3_Group7' folder
	- Select the 'cafes-and-restaurants-with-seating-capacity-modified_1.csv' file for the 'City Dining' dashboard
	- Select the 'melbourne-landmarks-pois-with-category.csv' file for the 'City Attractions' dashboard

Step 4: View the Dashboard
- After both R projects are running and the Tableau file is open, you should be able to see the dashboard populated with the processed data from both R projects.

#### Important Notes
Ensure that both R projects are running simultaneously while using Tableau to see real-time updates.