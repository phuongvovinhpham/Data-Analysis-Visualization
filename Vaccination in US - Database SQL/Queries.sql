/*Name: Phuong Pham*/
/*Student ID: 4078692*/

--Task D.1:
SELECT v1.OM1 AS "Observation Month 1 (OM1)",
    v1.CountryName AS "Country Name (CN)",
    v1.VOM1 AS "Administered Vaccine OM1 (VOM1)",
    v2.OM2 AS "Observation Month 2 (OM2)",
    v2.VOM2 AS "Administered Vaccine OM2 (VOM2)",
    (v1.VOM1 - v2.VOM2) AS "Difference of Totals (VOM1-VOM2)"
FROM (SELECT CountryName,
        strftime('%Y-%m', Date) AS OM1,
        SUM(DailyVaccination) AS VOM1
    FROM Vaccination
    WHERE Date BETWEEN '2022-04-01' AND '2022-04-30'
    GROUP BY CountryName, strftime('%Y-%m', Date)) v1
JOIN (SELECT CountryName,
        strftime('%Y-%m', Date) AS OM2,
        SUM(DailyVaccination) AS VOM2
    FROM Vaccination
    WHERE Date BETWEEN '2022-05-01' AND '2022-05-31'
    GROUP BY CountryName, strftime('%Y-%m', Date)) v2
ON v1.CountryName = v2.CountryName
ORDER BY v1.CountryName;


--Task D.2:
SELECT v1.CountryName AS "Country Name",
    v1.Month,
    v1.CumulativeDoses AS "Cumulative Doses"
FROM (SELECT CountryName,
        strftime('%Y-%m', Date) AS Month,
        MAX(PeopleVaccinated + PeopleFullyVacinated + TotalBooster) AS CumulativeDoses
    FROM Vaccination
    GROUP BY CountryName, strftime('%Y-%m', Date)) v1
JOIN (SELECT Month,
        AVG(CumulativeDoses) AS AvgCumulativeDoses
    FROM (SELECT strftime('%Y-%m', Date) AS Month,
            MAX(PeopleVaccinated + PeopleFullyVacinated + TotalBooster) AS CumulativeDoses
        FROM Vaccination
        GROUP BY CountryName, strftime('%Y-%m', Date))
    GROUP BY Month) v2
ON v1.Month = v2.Month
WHERE v1.CumulativeDoses > v2.AvgCumulativeDoses
ORDER BY v1.Month;


--Task D.3:
SELECT DISTINCT 
    VaccineName AS "Vaccine Type",
    CountryName AS "Country"
FROM Vaccines;


---Task D.4:
SELECT v.CountryName AS "Country Name",
    l.SourceURL AS "Source Name (URL)",
    SUM(v.DailyVaccination) AS "Total Administered Vaccines"
FROM Vaccination v
    JOIN Location l
    ON v.CountryName = l.CountryName
GROUP BY SourceURL
ORDER BY SUM(v.DailyVaccination);

--Task D.5:
SELECT Month AS "Date Range (Months)",
    MAX(CASE WHEN CountryName = 'United States' THEN TotalFullyVaccinated ELSE 0 END) AS "United States",
    MAX(CASE WHEN CountryName = 'Wales' THEN TotalFullyVaccinated ELSE 0 END) AS "Wales",
    MAX(CASE WHEN CountryName = 'Canada' THEN TotalFullyVaccinated ELSE 0 END) AS "Canada",
    MAX(CASE WHEN CountryName = 'Denmark' THEN TotalFullyVaccinated ELSE 0 END) AS "Denmark"
FROM (SELECT strftime('%Y-%m', Date) AS Month,
        CountryName,
        MAX(CASE WHEN PeopleFullyVacinated = '' THEN 0 ELSE PeopleFullyVacinated END) AS TotalFullyVaccinated
    FROM Vaccination 
    GROUP BY strftime('%Y-%m', Date), CountryName
    HAVING CountryName IN ('United States', 'Wales', 'Canada', 'Denmark')
        AND strftime('%Y', Date) IN ('2022', '2023'))
GROUP BY Month;
