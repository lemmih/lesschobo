CREATE VIEW CourseSelfStencils AS
  SELECT DISTINCT
    course_id, stencil_id
  FROM 
    Units,
    UnitMembers
  WHERE
    Units.id = UnitMembers.unit_id
  ORDER BY
    course_id, stencil_id;

CREATE OR REPLACE VIEW CourseMetrics AS
  SELECT
    Users.id as user_id, Courses.course_id,
    (SELECT COUNT(*)
      FROM CourseFeaturesM CourseFeatures, Models
      WHERE
        CourseFeatures.course_id = Courses.course_id AND
        CourseFeatures.feature_id = Models.feature_id AND
        Models.user_id = Users.id AND
        Models.at < now()) as review,
    (SELECT at
      FROM CourseFeaturesM CourseFeatures, Models
      WHERE
        CourseFeatures.course_id = Courses.course_id AND
        CourseFeatures.feature_id = Models.feature_id AND
        Models.user_id = Users.id AND
        Models.at > now()
      ORDER BY at ASC
      LIMIT 1) as change,
    (SELECT COUNT(*)
      FROM CourseSelfStencils CourseStencils, StencilLastSeen
      WHERE
        CourseStencils.course_id = Courses.course_id AND
        CourseStencils.stencil_id = StencilLastSeen.stencil_id AND
        StencilLastSeen.user_id = Users.id) as seen,
    (SELECT COUNT(*)
      FROM CourseSelfStencils CourseStencils
      WHERE
        CourseStencils.course_id = Courses.course_id) as total
  FROM
    Users,
    (SELECT DISTINCT course_id FROM Units) Courses;

