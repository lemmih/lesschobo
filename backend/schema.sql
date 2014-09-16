DROP EXTENSION IF EXISTS "uuid-ossp";
CREATE EXTENSION "uuid-ossp";

DROP TABLE IF EXISTS Stencils CASCADE;
DROP TABLE IF EXISTS StencilIssues CASCADE;
DROP TABLE IF EXISTS Features CASCADE;
DROP TABLE IF EXISTS StencilFeatures CASCADE;
DROP TABLE IF EXISTS Users CASCADE;
DROP TABLE IF EXISTS Responses CASCADE;
DROP TABLE IF EXISTS Models CASCADE;
DROP TABLE IF EXISTS Schedule CASCADE;
DROP TABLE IF EXISTS Units CASCADE;
DROP TABLE IF EXISTS UnitMembers CASCADE;
DROP TABLE IF EXISTS Inherit CASCADE;
DROP FUNCTION IF EXISTS post_stencil();
DROP VIEW IF EXISTS UserCourses CASCADE;
DROP VIEW IF EXISTS Courses CASCADE;

\set nullDate 1/1/0001

CREATE TABLE Users
  ( id text PRIMARY KEY
  );

CREATE TABLE Stencils
  ( id      uuid PRIMARY KEY
  , content text
  , UNIQUE (content)
  );

CREATE OR REPLACE VIEW StencilsView AS
  SELECT id, content
  FROM Stencils;

CREATE OR REPLACE FUNCTION post_stencil() RETURNS trigger AS
$$
DECLARE
  rec Stencils%ROWTYPE;
BEGIN
  LOOP
    SELECT id INTO rec FROM Stencils WHERE content = NEW.content;
    IF FOUND THEN
      NEW.id := rec.id;
      RETURN NEW;
    ELSE
      BEGIN
        INSERT INTO Stencils (id, content)
          VALUES (uuid_generate_v4(), NEW.content) RETURNING id INTO NEW.id;
        RETURN NEW;
      EXCEPTION
        WHEN unique_violation THEN
          NULL;
      END;
    END IF;
  END LOOP;
END;
$$
LANGUAGE plpgsql;

CREATE TRIGGER stencil_trigger
  INSTEAD OF INSERT ON StencilsView
  FOR EACH ROW
  EXECUTE PROCEDURE post_stencil();


CREATE TABLE StencilIssues
  ( stencil_id uuid REFERENCES Stencils(id)
  , created_by text REFERENCES Users(id)
  , created_at timestamp
  , issue      text
  );

CREATE TABLE Features
  ( id      uuid PRIMARY KEY
  , content text
  );

CREATE OR REPLACE VIEW FeaturesView AS
  SELECT id, content
  FROM Features;

CREATE OR REPLACE FUNCTION post_feature() RETURNS trigger AS
$$
DECLARE
  rec Features%ROWTYPE;
BEGIN
  LOOP
    SELECT id INTO rec FROM Features WHERE content = NEW.content;
    IF FOUND THEN
      NEW.id := rec.id;
      RETURN NEW;
    ELSE
      BEGIN
        INSERT INTO Features (id, content)
          VALUES (uuid_generate_v4(), NEW.content) RETURNING id INTO NEW.id;
        RETURN NEW;
      EXCEPTION
        WHEN unique_violation THEN
          NULL;
      END;
    END IF;
  END LOOP;
END;
$$
LANGUAGE plpgsql;

CREATE TRIGGER feature_trigger
  INSTEAD OF INSERT ON FeaturesView
  FOR EACH ROW
  EXECUTE PROCEDURE post_feature();







CREATE TABLE StencilFeatures
  ( stencil_id uuid REFERENCES Stencils(id)
  , feature_id uuid REFERENCES Features(id)
  , UNIQUE (stencil_id, feature_id)
  );

CREATE TABLE Responses
  ( id         uuid PRIMARY KEY
  , stencil_id uuid REFERENCES Stencils(id)
  , user_id    text REFERENCES Users(id)
  , type       text
  , content    text
  , at         timestamp
  ); 

CREATE OR REPLACE VIEW StencilLastSeen AS
  SELECT user_id, stencil_id, max(at) as seen_at
  FROM Responses
  GROUP BY user_id, stencil_id;

CREATE TABLE Models
  ( user_id    text REFERENCES Users(id)
  , feature_id uuid REFERENCES Features(id)
  , type       text
  , content    text
  , at         timestamp
  , UNIQUE (user_id, feature_id)
  );

-- CREATE TABLE Schedule
--   ( user_id    text REFERENCES Users(id)
--   , stencil_id uuid REFERENCES Stencils(id)
--   , at         timestamp NULL
--   , UNIQUE ( user_id, stencil_id )
--   );

CREATE OR REPLACE VIEW UserStencils AS
  SELECT Users.id as user_id, Stencils.id as stencil_id
  FROM Users, Stencils;

CREATE OR REPLACE View UserFeatures AS
  SELECT user_id, us.stencil_id, feature_id
  FROM UserStencils us, StencilFeatures sf
  WHERE us.stencil_id = sf.stencil_id;

CREATE OR REPLACE VIEW Schedule AS
  SELECT uf.user_id, stencil_id, min(coalesce(at, :'nullDate')) as at
  FROM UserFeatures uf
  LEFT JOIN Models m
  ON m.user_id = uf.user_id AND m.feature_id = uf.feature_id
  GROUP by uf.user_id, stencil_id;

-- Unit id and course id are 'text' becaue they're managed by meteor/mongo.
-- They are guaranteed to be unique.
CREATE TABLE Units
  ( id        text PRIMARY KEY
  , course_id text
  , index     int
  , UNIQUE (course_id, index)
  );

CREATE TABLE UnitMembers
  ( unit_id    text REFERENCES Units(id)
  , stencil_id uuid REFERENCES Stencils(id)
  , index      int
  );

-- Receivers inherit all of the stencils+features of their givers when
-- reviewing. Inheritance doesn't change how new material is introduced.
CREATE TABLE Inherit
  ( receiver text
  , giver text
  , UNIQUE (receiver, giver)
  );

CREATE VIEW UserCourses AS
  SELECT
    Users.id as user_id,
    Units.course_id, Units.id as unit_id, Units.index AS UnitIndex,
    UnitMembers.index AS StencilIndex, UnitMembers.stencil_id
  FROM
    Units,
    UnitMembers,
    Users
  WHERE
    Units.id = UnitMembers.unit_id;

CREATE VIEW Courses AS
  SELECT
    UserCourses.user_id,
    course_id, unit_id, UnitIndex,
    StencilIndex, UserCourses.stencil_id,
    at
  FROM
    UserCourses
  LEFT JOIN Schedule
  ON
    Schedule.user_id = UserCourses.user_id AND
    Schedule.stencil_id = UserCourses.stencil_id;

CREATE VIEW CourseIssues AS
  SELECT
    course_id, StencilIssues.stencil_id,
    created_by, created_at,
    issue
  FROM
    Units,
    UnitMembers,
    StencilIssues
  WHERE
    Units.id = UnitMembers.unit_id AND
    UnitMembers.stencil_id = StencilIssues.stencil_id;

-- CREATE OR REPLACE VIEW ReviewItems AS
--   SELECT user_id, course_id, unit_id, stencil_id, feature_id
--   FROM UserCourses u, Schedule s
--   WHERE u.user_id = s.user_id AND u.stencil_id = s.stencil_id
-- ReviewItems
-- ( user_id, course_id, unit_id, stencil_id, last_seen, feature_id, at )


\set id1 58ab0355-8d73-4034-bfd6-97631b1af133
\set id2 da613059-25f0-4df7-bffa-2cf465a22c34
\set id3 833f76f2-4f7e-435e-89dc-f67587b612b4
\set id4 e38c1d26-ddc3-49d8-a38e-ef193e743678
\set id5 abc14fb7-66d9-45ff-8093-a3340b0817d7
\set id6 206a9700-83dd-422a-ae40-5509ebbeac6f
\set id7 8cca20b8-9f9c-46e0-a16a-bbd727f3fa7b
\set id8 7e502817-fcb8-44d8-bd79-04b8e2557af8
\set id9 e331a3b0-8d54-4bbb-8034-4a1ec7dae4ae

INSERT INTO Stencils VALUES ( :'id1' );
INSERT INTO Stencils VALUES ( :'id2' );
INSERT INTO Stencils VALUES ( :'id3' );
INSERT INTO Stencils VALUES ( :'id4' );

INSERT INTO Features VALUES ( :'id5', 'f1');
INSERT INTO Features VALUES ( :'id6', 'f2');

INSERT INTO StencilFeatures VALUES ( :'id1', :'id5' );
INSERT INTO StencilFeatures VALUES ( :'id1', :'id6' );
INSERT INTO StencilFeatures VALUES ( :'id2', :'id5' );

INSERT INTO Units VALUES ( 'Unit 1' , 'Course 1', 1 );
INSERT INTO Units VALUES ( 'Unit 2' , 'Course 1', 2 );
INSERT INTO Units VALUES ( 'Unit A' , 'Course 2', 1 );

INSERT INTO Inherit VALUES ( 'Course 1', 'Course 1' );
INSERT INTO Inherit VALUES ( 'Course 2', 'Course 2' );
INSERT INTO Inherit VALUES ( 'Course 2', 'Course 1' );

INSERT INTO UnitMembers VALUES ( 'Unit 1', :'id1', 1 );
INSERT INTO UnitMembers VALUES ( 'Unit 1', :'id2', 2 );
INSERT INTO UnitMembers VALUES ( 'Unit 2', :'id3', 1 );
INSERT INTO UnitMembers VALUES ( 'Unit A', :'id1', 1 );
INSERT INTO UnitMembers VALUES ( 'Unit A', :'id4', 1 );

INSERT INTO Users VALUES ( 'user' );
INSERT INTO Users VALUES ( 'user2' );


INSERT INTO Responses VALUES
  ( :'id7', :'id1', 'user', 'type', 'content', '2010-10-20' );
INSERT INTO Responses VALUES
  ( :'id8', :'id1', 'user', 'type', 'content', '2014-09-14' );
INSERT INTO Responses VALUES
  ( :'id9', :'id2', 'user', 'type', 'content', '2014-09-15' );


INSERT INTO Models VALUES ( 'user', :'id5', '', '', now() );
INSERT INTO Models VALUES ( 'user', :'id6', '', '', now() );

-- INSERT INTO Schedule VALUES ( 'user', :'id1', now() );
-- INSERT INTO Schedule VALUES ( 'user', :'id3', now() );

-- INSERT INTO Schedule VALUES ( 'user2', :'id1', now() );

INSERT INTO StencilIssues VALUES ( :'id1', 'user', now(), 'issue!' );

CREATE OR REPLACE VIEW CourseFeatures AS
  SELECT DISTINCT
    Inherit.receiver as course_id, StencilFeatures.stencil_id, feature_id
  FROM 
    Units,
    UnitMembers,
    Inherit,
    StencilFeatures
  WHERE
    Inherit.giver = Units.course_id AND
    Units.id = UnitMembers.unit_id AND
    UnitMembers.stencil_id = StencilFeatures.stencil_id;

CREATE OR REPLACE VIEW Review AS
  SELECT DISTINCT ON (user_id, course_id, stencil_id)
    Users.id as user_id, course_id,
    CourseFeatures.stencil_id, seen_at,
    CourseFeatures.feature_id, at as review_at
  FROM
    Users,
    CourseFeatures,
    Models,
    StencilLastSeen
  WHERE
    Users.id = Models.user_id AND
    CourseFeatures.feature_id = Models.feature_id AND
    StencilLastSeen.stencil_id = CourseFeatures.stencil_id
  ORDER BY
    user_id, course_id, stencil_id, feature_id, seen_at ASC;

/*
Introduce new material:
Give me the list of stencils for this course up to unit X which have not
been practiced.

Review material:
Find the features for a given course, sort them by review date, limit to 10.
Find the stencils available for this course that use any of the selected
features AND have a review date no earlier than the review date for the
feature. Sort and filter the results such that only one stencil is selected
per feature and the selected stencil has the least recent seen date.

Metrics:
  Global and per course:
    How many features have review date in the past?
    How many features have review date in the future?
    How many stencils have review date in the past?
    How many stencils have review date in the future?
*/
