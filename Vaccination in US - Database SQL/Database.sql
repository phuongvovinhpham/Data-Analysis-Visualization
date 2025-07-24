/*Name: Phuong Pham*/
/*Student ID: 4078692*/

PRAGMA foreign_keys = ON;

CREATE TABLE Location (
    CountryName VARCHAR(30) NOT NULL PRIMARY KEY,
    CountryCode VARCHAR(30) NOT NULL,
    SourceName VARCHAR(30),
    SourceURL VARCHAR(30)
    );
    
CREATE TABLE Observation (
    Date TEXT NOT NULL PRIMARY KEY
    );

CREATE TABLE Age_Group (
    CountryName VARCHAR(30) NOT NULL,
    Date TEXT NOT NULL,
    AgeGroup VARCHAR(30) NOT NULL,
    FOREIGN KEY (Date) REFERENCES Observation (Date),
    FOREIGN KEY (CountryName) REFERENCES Location (CountryName),
    PRIMARY KEY (CountryName, Date, AgeGroup)
    );
    
CREATE TABLE Vaccines (
    CountryName VARCHAR(30) NOT NULL,
    Date TEXT NOT NULL,
    VaccineName VARCHAR(30) NOT NULL,
    TotalVaccination INTEGER NOT NULL,
    FOREIGN KEY (Date) REFERENCES Observation (Date),
    FOREIGN KEY (CountryName) REFERENCES Location (CountryName),
    PRIMARY KEY (CountryName, Date, VaccineName)
    ); 
    

CREATE TABLE Vaccination (
    CountryName VARCHAR(30) NOT NULL,
    Date TEXT NOT NULL,
    PeopleVaccinated INTEGER,
    PeopleFullyVacinated INTEGER,
    TotalBooster INTEGER,
    DailyVaccination INTEGER,
    FOREIGN KEY (CountryName) REFERENCES Location (CountryName),
    FOREIGN KEY (Date) REFERENCES Observation (Date),
    PRIMARY KEY (CountryName, Date)
    );

    
CREATE TABLE United_States (
    CountryName VARCHAR(30) NOT NULL,
    State VARCHAR(30) NOT NULL,
    FOREIGN KEY (CountryName) REFERENCES Location (CountryName),
    PRIMARY KEY (CountryName, State)
    );

