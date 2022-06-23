.read data.sql


CREATE TABLE bluedog AS
  SELECT color, pet FROM students
  WHERE color = 'blue' and pet = 'dog';

CREATE TABLE bluedog_songs AS
  SELECT color, pet, song FROM students
  WHERE color = 'blue' and pet = 'dog';


CREATE TABLE matchmaker AS
  SELECT first.pet, first.song, first.color, second.color
  FROM students AS first, students AS second
  WHERE first.time < second.time and first.pet = second.pet and first.song = second.song;


CREATE TABLE sevens AS
  SELECT seven FROM students, numbers
  WHERE students.time = numbers.time and students.number = 7 and numbers.'7' = 'True';


CREATE TABLE favpets AS
  SELECT pet, COUNT(*) AS count FROM students
  GROUP BY pet ORDER BY count DESC LIMIT 10; 


CREATE TABLE dog AS
  SELECT pet, COUNT(*) FROM students
  WHERE pet = 'dog';


CREATE TABLE bluedog_agg AS
  SELECT song, COUNT(*) FROM bluedog_songs
  GROUP BY song ORDER BY COUNT(*) DESC;


CREATE TABLE instructor_obedience AS
  SELECT seven, instructor, COUNT(*) FROM students
  WHERE seven = '7'
  GROUP BY instructor ORDER BY COUNT(*) DESC; 

