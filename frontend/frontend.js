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

  function fetchMoreCards(courseId) {
    Session.set('stencilId', undefined);
    Session.set('sentences', undefined);
    Session.set('cards', undefined);
    Meteor.call('fetchCards', courseId, function (err, data) {
        console.log('Got data', data);
        Session.set('stencilId', data[0].stencilId);
        Session.set('sentences', data.shift().sentences);
        Session.set('cards', data);
        Template.interface.setActive();
      });
  }

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

  Template.interface.created = function () {
    console.log('created', this.data);
    fetchMoreCards(this.data.courseId);
  };
  Template.interface.isCompleted = function () {
    var s = (Session.get('sentences')||[]);
    for(var i=0; i<s.length; i++) {
      for(var j=0; j<s[i].blocks.length;j++) {
        if (s[i].blocks[j].isActive) {
          return false;
        }
      }
    }
    return true;
  };
  Template.interface.sentences = function () {
    return (Session.get('sentences')||[]);
  };
  Template.interface.dictionary = function () {
    var s = (Session.get('sentences')||[]);
    for(var i=0; i<s.length; i++) {
      for(var j=0; j<s[i].blocks.length;j++) {
        if (s[i].blocks[j].isActive) {
          return s[i].blocks[j].definitions[0].english.join('/').split('/').filter(function(s){return s.length!=0;});
        }
      }
    }
    return [];
  }
  Template.interface.normalise = function () {
    withSentences(function (s) {
      for(var i=0; i<s.length; i++) {
        for(var j=0; j<s[i].blocks.length;j++) {
          s[i].blocks[j].sentenceId = i;
          s[i].blocks[j].blockId    = j;
          s[i].blocks[j].len   = s[i].blocks[j].isEscaped ? 0 : s[i].blocks[j].chinese.length;
          s[i].blocks[j].literal = false;
          s[i].blocks[j].dictIndex = 0;
        }
      }
    });
  };
  Template.interface.dictEntries = function () {
    var s = Session.get('sentences')||[];
    var i = this.sentenceId;
    var j = this.blockId;
    return s[i].blocks[j].definitions.map(function (def, idx) {
      return {
        pinyin: def.pinyin,
        english: def.english.map(function (entry) {
          return {
            sentenceId:   i,
            blockId:      j,
            definitionId: idx,
            english:      entry };
          })
      };
    });
    // return s[i].blocks[j].definitions[0].english
    //           .map(function(s){return {sentenceId: i, blockId: j, text: s};});
  }
  Template.interface.thisPinyin = function () {
    var s = Session.get('sentences')||[];
    var i = this.sentenceId;
    var j = this.blockId;
    var dictIdx = s[i].blocks[j].dictIndex;
    return s[i].blocks[j].definitions[dictIdx].pinyin;
  }
  Template.interface.setActive = function () {
    Template.interface.normalise();
    withSentences(function (s) {
      for(var i=0; i<s.length; i++) {
        for(var j=0; j<s[i].blocks.length;j++) {
          if (s[i].blocks[j].isGap) {
            s[i].blocks[j].isActive = true;
            return;
          } else {
            s[i].blocks[j].isActive = false;
          }
        }
      }
    });
  };
  Template.interface.withActive = function (fn) {
    return withSentences(function (s) {
      for(var i=0; i<s.length; i++) {
        for(var j=0; j<s[i].blocks.length;j++) {
          if (s[i].blocks[j].isActive) {
            return fn(s[i].blocks[j]);
          }
        }
      }
    });
  };
  Template.interface.unsetActive = function () {
    Template.interface.withActive(function (activeBlock) {
      activeBlock.isActive = false;
      activeBlock.isGap    = false;
    });
  };
  Template.interface.doShowAnswer = function () {
    return Session.get('showAnswer') || false;
  }
  Template.interface.answer = function () {
    return Template.interface.withActive(function (activeBlock) {
      // return activeBlock.chinese + ' (' + activeBlock.pinyin + ')';
      return activeBlock.definitions[activeBlock.dictIndex].pinyin;
    });
  }
  Template.interface.characters = function () {
    return Template.interface.withActive(function (activeBlock) {
      return activeBlock.chinese;
    });
  }
  Template.interface.idiomatic = function () {
    return (Session.get("answer")|| "");
  };
  Template.interface.transliterationClass = function () {
    return (Session.get("showTransliteration") ? "visible" : "hidden");
  };
  Template.interface.events({
    'keyup #answer': function (evt) {
      Session.set("answer", evt.currentTarget.value);
    },
    'keydown .activeInputInline': function (evt) {
      var active;
      if (evt.keyCode === 27) {
        Session.set('showAnswer', true);
      } else if(evt.keyCode===13) {
        console.log('submit');
        Template.interface.withActive(function (activeBlock) {
          var userAnswer = $('.activeInputInline').val();
          var response = {stencilId: Session.get('stencilId'),
                          content: {
                            type: 'MandarinTextAnswer',
                            shownAnswer: Template.interface.doShowAnswer(),
                            key: activeBlock.chinese,
                            value: userAnswer },
                          at: (new Date().toJSON())
                         };
          Meteor.call('postResponse', response);

          if (userAnswer === activeBlock.chinese) {

            setTimeout(function () {
              Template.interface.unsetActive();
              Template.interface.setActive();
            }, 0);
            Session.set('showAnswer', false);
            $('.activeInputInline').value = '';
          } else {
            console.log('incorrect', userAnswer, activeBlock.chinese);
            $('.activeInputInline').select();
          }
        });
      }
    },
    'click .dict-select': function (evt) {
      var self = this;
      withSentences(function (s) {
        s[self.sentenceId].blocks[self.blockId].literal = self.english;
        s[self.sentenceId].blocks[self.blockId].dictIndex = self.definitionId;
      });
    },
    'click .literal': function (evt) {
      var self = this;
      withSentences(function (s) {
        s[self.sentenceId].blocks[self.blockId].literal = '';
      });
    },
    'click #nextCardButton': function () {
      var cards = Session.get('cards');
      if( cards.length === 0 ) {
        console.log('fetch next', this.courseId);
        fetchMoreCards(this.courseId);
      } else {
        Session.set('stencilId', cards[0].stencilId);
        Session.set('sentences', cards.shift().sentences);
        Session.set('cards', cards);
        Template.interface.setActive();
      }
    },
    'click #fetchCards': function () {
      // Meteor.call('fetchCards', this.courseId, function (err, data) {
      //   console.log('Got data', data);
      //   Session.set('stencilId', data[0].stencilId);
      //   Session.set('sentences', data.shift().sentences);
      //   Session.set('cards', data);
      //   Template.interface.setActive();
      // });
    },
    'click #toggleTransliteration': function () {
      if($(".literal").css('opacity') === "1") {
        $(".literal").animate({opacity: 0, width: 0});
      } else {
        $(".literal").animate({opacity: 1, width: "100%"});
      }
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
