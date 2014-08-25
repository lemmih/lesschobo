Template.review.created = function () {
  console.log('studyCourse', this.data);
  cards.remove({});
  fetchMoreCards(this.data);
}
Template.review.studyTemplate = function () {
  if ( !cards.empty() )
    return 'study';
  if ( Session.equals('loading', true) )
    return 'studyLoading';
  // empty(cards) === true
  return 'studyFinished';
}

Template.studyCourse.created = function () {
  console.log('studyCourse', this.data);
  cards.remove({});
  fetchMoreCards(this.data);
}
Template.studyCourse.studyTemplate = function () {
  if ( !cards.empty() )
    return 'study';
  if ( Session.equals('loading', true) )
    return 'studyLoading';
  // empty(cards) === true
  return 'studyFinished';
}


Template.study.cardTemplate = function () {
  var activeId = Session.get('activeCard');
  if ( activeId === 'final' )
    return 'final';
  var card = activeCard('cardTemplate');
  if ( !card ) return 'studySprintCompleted';
  return 'studyMandarinCard';
}


Template.view.title = function () {
  if( !this.course ) return '';
  var idx = parseInt(this.unitIndex);
  return this.course.units[idx].title;
}
Template.view.content = function () {
  if( !this.course ) return '';
  var idx = parseInt(this.unitIndex);
  return this.course.units[idx].content || 'No content written for unit.';
}


Template.learnToolbar.cards = function () {
  return cards.find().fetch();
};
Template.learnToolbar.cardClass = function () {
  var activeId = Session.get('activeCard');
  var active = activeId == this.value._id ? 'active' : '';
  switch( this.value.status ) {
    case 'active': return active + ' next';
    case 'blocked': return active + ' disabled';
    case 'completed': return active + ' btn-success'
    default: return '';
  }
};
Template.learnToolbar.final = function () {
  var activeId = Session.get('activeCard');
  var newCard = cards.findOne({status: "active"});
  if ( !newCard ) {
    if( activeId === 'final' )
      return 'next btn-info active';
    return 'next btn-info';
  }
  return 'disabled'
};
Template.learnToolbar.events({
  'click .learn-toolbar-card': function (evt) {
    Session.set('activeCard', this.value._id);
    console.log(this.value._id);
    //console.log($(evt.target).attr('data-key'));
  },
  'click .learn-toolbar-final': function (evt) {
    Session.set('activeCard', 'final');
    //console.log($(evt.target).attr('data-key'));
  },
  'click #showAnswerBtn': function (evt) {
    activeCard('showAnswerBtn').showAnswer();
  }
});

Template.final.created = function () {
  console.log('final created', this.data.action);
  fetchMoreCards(this.data);
};
Template.final.events({
  'click .continue': function (evt) {
    console.log('final clicked', this.action);
    activateNextCardSet(this.courseId);
  }
});



Template.learnLayout.activeWhenEq = function (a, b) {
  return a == b ? "active" : "";
};

Template.studyNav.nextIdx = function () {
  if( !this.course ) return 0;
  var idx = parseInt(this.unitIndex);
  return idx+1;
};
Template.studyNav.nextActive = function () {
  if( !this.course ) return 0;
  var idx = parseInt(this.unitIndex);
  var nUnits = this.course.units.length;
  return idx+1 < nUnits ? "" : "disabled";
};

Template.studyNav.prevIdx = function () {
  if( !this.course ) return 0;
  var idx = parseInt(this.unitIndex);
  return idx-1;
};
Template.studyNav.prevActive = function () {
  if( !this.course ) return 0;
  var idx = parseInt(this.unitIndex);
  return idx != 0 ? "" : "disabled";
};