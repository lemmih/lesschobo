var cards = new Meteor.Collection(null);

fetchMoreCards = function(courseId) {
  Session.set('stencilId', undefined);
  Session.set('sentences', undefined);
  Session.set('cards', undefined);
  Meteor.call('fetchCards', courseId, function (err, data) {
      console.log('Got data', data);
      instantiateCards(data);
      cards.remove({});
      for(var i=0; i<data.length; i++)
        cards.insert(data[i]);
      Session.set('activeCard', cards.findOne()._id);
    });
}
// Instantiate extra card fields which are used for teaching logic.
function instantiateCards(cards) {
  for(var i=0; i<cards.length; i++) {
    cards[i].status = 'blocked';
    switch (cards[i].type) {
      case "chinese":
        instantiateChineseCard(cards[i]);
        break;
      default:
        console.log('instatiateCards: unhandled card type', cards[i].type);
        break;
    }
  }
  if( cards.length > 0)
    cards[0].status = 'active';
}

activeCard = function () {
  var activeId = Session.get('activeCard');
  return cards.findOne({_id: activeId});
}
saveCard = function (card) {
  cards.update({_id: card._id}, card);
}
nextCard = function () {
  var activeId = Session.get('activeCard');
  var newCard = cards.findOne({status: "blocked"});
  newCard.status = 'active';
  saveCard(newCard);
  Session.set('activeCard', newCard._id);
}


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
      for(var j=0; j<s[i].blocks.length; j++) {
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




// Templating related to study





Template.study.created = function () {
  console.log('created', this.data);
  fetchMoreCards(this.data.courseId);
};
Template.study.rendered = function () {
  $("#study-nav").slimScroll({height: 'auto'});
  $(window).resize(function(){
    $("#study-nav").slimScroll({height: 'auto'});
  });
}

Template.studyProgress.cards = function() { return cards.find(); };
