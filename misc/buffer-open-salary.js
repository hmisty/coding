/**
 * Print all salaries of Buffer inc.
 * Evan 2015
 */
var unit = "USD";
var role = {
  "Happiness Hero": 45000,
  "Content Crafter": 50000,
  "Engineer": 60000,
  "Desinger": 60000,
  "Operations Officer": 70000,
  "Executive Officer": 75000
};
var revenue = 1; //million USD
var seniority = {
  "-": [0, 0],
  "Senior": [0.05, 3000],
  "Lead": [0.07, 4000],
  "VP": [0.10, 6000],
  "C-Level": [0.20, 8000],
  "COO": [0.20, 10000],
  "CEO": [0.20, 12000]
};
var experience = {
  "Junior": 1,
  "Intermediate": 1.1,
  "Advanced": 1.2,
  "Master": 1.3
};
var location = {
  "A": 22000,
  "B": 12000,
  "C": 6000,
  "D": 0
};
var salaryChoice = {
  "Salary Choice": 10000,
  "Equity Choice": 0
};

for (var r in role) {
  for (var s in seniority) {
    for (var e in experience) {
      for (var l in location) {
        for (var c in salaryChoice) {
          var base = role[r] * (1 + seniority[s][0]);
          var salary = base * experience[e] + seniority[s][1] * revenue + location[l] + salaryChoice[c];
          console.log("%s, %s, %s, %s, %s => %s",
                      r, s, e, l, c,
                      salary);
        }
      }
    }
  }
}
