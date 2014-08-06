if ( Meteor.isClient ) {
  Handlebars.registerHelper('key_value', function(context, options) {
    var result = [];
    _.each(context, function(value, key, list){
      result.push({key:key, value:value});
    })
    return result;
  });
}



if (Meteor.isClient) {
  jQuery.fn.animateAuto = function(prop, speed, callback){
      var elem, height, width;
      return this.each(function(i, el){
          el = jQuery(el), elem = el.clone().css({"height":"auto","width":"auto"}).appendTo("body");
          height = elem.css("height"),
          width = elem.css("width"),
          elem.remove();

          if(prop === "height")
              el.animate({"height":height}, speed, callback);
          else if(prop === "width")
              el.animate({"width":width}, speed, callback);
          else if(prop === "both")
              el.animate({"width":width,"height":height}, speed, callback);
      });
  }
}

if (!Meteor.isClient) {
  Meteor.methods({
    fetchCards: function (courseId) {
      var ret = HTTP.call('GET', 'http://localhost:8000/users/' + Meteor.userId() + '/units/' + courseId + '/cards');
      return JSON.parse(ret.content);
    },
    fetchAnnotatedStencils: function (courseId) {
      console.log('userId', Meteor.userId());
      var ret = HTTP.call('GET', 'http://localhost:8000/users/' + Meteor.userId() + '/units/' + courseId + '/stencils/');
      return JSON.parse(ret.content);
    },
    postResponse: function(response) {
      response.userId = Meteor.userId();

      return HTTP.call('POST', 'http://localhost:8000/responses', {data: response, params: {key: 'value'}});
    },
    listUnits: function() {
      var ret = HTTP.call('GET', 'http://localhost:8000/users/' + Meteor.userId() + '/units/');
      return JSON.parse(ret.content);
    }
  });
}

if (Meteor.isClient) {

  function withSentences(fn) {
    var s = Session.get('sentences') || [];
    var ret = fn(s);
    Session.set('sentences', s);
    return ret;
  };


  Template.home.params = function () {
    return {_id: 3520};
  };
  Template.home.courseList = function () {
    console.log('computing course list');
    return [{_id: 3520, name: 'Chinese'}];
  };
  Template.home.courseLinkParams = function () {
    return {id: this.id, slug: this.unit.slug};
  };



  Template.viewCourse.created = function () {
    var courseId = this.data.courseId;
    console.log('Fetching stencils...', courseId);
    Session.set('stencils', undefined);
    Meteor.call('fetchAnnotatedStencils', courseId, function (err, data) {
      console.log('Got stencils:', data);
      // for(var i=0;i<data.length;i++) {
      //   if( data[i].schedule ) {
      //     var parsed = moment(data[i].schedule);
      //     data[i].scheduleETA  = parsed.fromNow();
      //     data[i].schedulePP   = parsed.calendar();
      //     data[i].scheduleUnix = parsed.unix();
      //   } else {
      //     data[i].scheduleETA  = '';
      //     data[i].schedulePP   = '';
      //     data[i].scheduleUnix = 0;
      //   }
      // }
      Session.set('stencils', data);
      Template.viewCourse.setStencilsCache();
      Meteor.setInterval(Template.viewCourse.setStencilsCache, 10000);
    });
  };
  Template.viewCourse.setStencilsCache = function () {
    var data = Session.get('stencils')||[];
    for(var i=0;i<data.length;i++) {
      if( data[i].schedule ) {
        var parsed = moment(data[i].schedule);
        data[i].scheduleETA  = parsed.fromNow();
        data[i].schedulePP   = parsed.calendar();
        data[i].scheduleUnix = parsed.unix();
      } else {
        data[i].scheduleETA  = '';
        data[i].schedulePP   = '';
        data[i].scheduleUnix = 0;
      }
    }
    Session.set('stencils', data);
  };
  Template.viewCourse.stencils = function () {
    var ordering = Session.get('ordering')||'order';
    var stencils = Session.get('stencils')||[];
    if( ordering === 'date' ) {
      return stencils.sort(function (a,b) {
        if( a.schedule === b.schedule ) return a.order - b.order;
        if( a.schedule === null ) return 1;
        if( b.schedule === null ) return -1;
        return a.scheduleUnix - b.scheduleUnix;
      });
    } else {
      return stencils;
    }

  };
  Template.viewCourse.getETA = function (time) {
    return '';//time?time.fromNow():'';
  };
  Template.viewCourse.ppTime = function (time) {
    console.log('ppTime', time);
    return ''; // time?time.calendar():'';
  };
  Template.viewCourse.events({
    'click .order': function (evt) {
      Session.set('ordering', 'order');
    },
    'click .time': function (evt) {
      Session.set('ordering', 'date');
    }
  });


  Template.layout.events({
    'click #logout': function () {
      console.log('logout');
      Meteor.logout();
    }
  });
  Template.layout.isOnPage = function (page) {
    var curPage = Router.current().route.name;
    if( page === curPage ) return 'active';
    return null;
  };






}

if (Meteor.isServer) {
  Meteor.startup(function () {
    // code to run on server at startup
  });
}
