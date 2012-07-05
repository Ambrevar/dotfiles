// ==UserScript==
// @name		ViewTube
// @version		2012.05.19
// @namespace		sebaro
// @description		Watch videos from video sharing websites without Flash Player.
// @include		http://youtube.com*
// @include		http://www.youtube.com*
// @include		https://youtube.com*
// @include		https://www.youtube.com*
// @include		http://dailymotion.com*
// @include		http://www.dailymotion.com*
// @include		https://dailymotion.com*
// @include		https://www.dailymotion.com*
// @include		http://metacafe.com*
// @include		http://www.metacafe.com*
// @include		https://metacafe.com*
// @include		https://www.metacafe.com*
// @include		http://vimeo.com*
// @include		http://www.vimeo.com*
// @include		https://vimeo.com*
// @include		https://www.vimeo.com*
// @include		http://break.com*
// @include		http://www.break.com*
// @include		https://break.com*
// @include		https://www.break.com*
// @include		http://sports.break.com*
// @include		https://sports.break.com*
// @include		http://funnyordie.com*
// @include		http://www.funnyordie.com*
// @include		https://funnyordie.com*
// @include		https://www.funnyordie.com*
// @include		http://blip.tv*
// @include		http://www.blip.tv*
// @include		https://blip.tv*
// @include		https://www.blip.tv*
// @include		http://veoh.com*
// @include		http://www.veoh.com*
// @include		https://veoh.com*
// @include		https://www.veoh.com*
// @include		http://veehd.com*
// @include		http://www.veehd.com*
// @include		https://veehd.com*
// @include		https://www.veehd.com*
// @include		http://mevio.com*
// @include		http://*.mevio.com*
// @include		https://mevio.com*
// @include		https://*.mevio.com*
// @include		http://5min.com*
// @include		http://www.5min.com*
// @include		https://5min.com*
// @include		https://www.5min.com*
// @include		http://videojug.com*
// @include		http://www.videojug.com*
// @include		https://videojug.com*
// @include		https://www.videojug.com*
// @include		http://facebook.com*
// @include		http://www.facebook.com*
// @include		https://facebook.com*
// @include		https://www.facebook.com*
// @include		http://imdb.com/video*
// @include		http://www.imdb.com/video*
// @include		https://imdb.com/video*
// @include		https://www.imdb.com/video*
// @include		http://dsc.discovery.com/videos*
// @include		https://dsc.discovery.com/videos*
// @include		http://video.nationalgeographic.com*
// @include		https://video.nationalgeographic.com*
// ==/UserScript==


/*
  
  Copyright (C) 2010 - 2012 Sebastian Luncan

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
  
  Site: http://isebaro.com/viewtube
  Contact: http://isebaro.com/contact
  
*/


(function() {


// ==========Variables========== //

// Page
var page = {win: window, doc: document, body: document.body, url: window.location.href};

// Player
var player = {};
var feature = {'autoplay': true, 'definition': true, 'container': false, 'fullsize': true};
var option = {'plugin': 'Auto', 'autoplay': false, 'autoget': false, 'definition': 'HD', 'container': 'MP4'};
var plugins = ['Auto', 'MPEG', 'MP4', 'HTML5'];
if (navigator.platform.indexOf('Win') != -1) plugins = plugins.concat(['WMP', 'WMP2', 'QT']);
else if (navigator.platform.indexOf('Mac') != -1) plugins = plugins.concat(['QT']);
var mimetypes = {'MPEG': 'video/mpeg', 'MP4': 'video/mp4', 'WMP': 'application/x-ms-wmp', 'WMP2': 'application/x-mplayer2', 'QT': 'video/quicktime'};

// Messages
var contact = 'http://isebaro.com/contact/?ln=en&sb=viewtube';


// ==========Fixes========== //

// Don't run on frames or iframes
if (window.top != window.self)  return;

 
// ==========Functions========== //

function createMyElement (type, content, event, action, target) {
  var obj = page.doc.createElement(type);
  if (content) {
    if (type == 'div' || type == 'option') obj.innerHTML = content;
    else if (type == 'img') obj.src = content;
    else if (type == 'video') {
      obj.src = content;
      obj.innerHTML = '<br><br>The video should be loading. If it doesn\'t load, make sure your browser supports HTML5\'s Video and this video codec. If you think it\'s a script issue, please report it <a href="' + contact + '">here</a>.';
    }
    else if (type == 'object') {
      obj.width = player['contentWidth'];
      obj.height = player['contentHeight'];
      obj.data = content;
      obj.innerHTML = '<br><br>The video should be loading. If it doesn\'t load, make sure a video plugin is installed. If you think it\'s a script issue, please report it <a href="' + contact + '">here</a>.<param name="scale" value="tofit"><param name="scale" value="exactfit"><param name="stretchtofit" value="true"><param name="autostart" value="true"><param name="autoplay" value="true">';
    }
  }
  if (type == 'video' || type == 'object') {
    if (option['plugin'] == 'Auto' || option['plugin'] == 'HTML5') {
      if (player['videoPlay'].indexOf('FLV') != -1) obj.type = 'video/x-flv';
      else if (player['videoPlay'].indexOf('MP4') != -1) obj.type = 'video/mp4';
      else if (player['videoPlay'].indexOf('WebM') != -1) obj.type = 'video/webm';
      else if (player['videoPlay'].indexOf('MOV') != -1) obj.type = 'video/quicktime';
      else if (player['videoPlay'].indexOf('M4V') != -1) obj.type = 'video/x-m4v';
      else if (player['videoPlay'].indexOf('AVI') != -1) obj.type = 'video/x-msvideo';
    }
    else {
      obj.type = mimetypes[option['plugin']];
    }
  }
  if (type == 'video') {
    obj.controls = 'controls';
    obj.autoplay = 'autoplay';
  }
  if (event == 'change') {
    if (target == 'video') {
      obj.addEventListener ('change', function () {
	player['videoPlay'] = this.value;
	if (!option['autoget']) modifyMyElement (player['buttonGet'] , 'div', 'Get »', false);
	playMyVideo(option['autoplay']);
      }, false);
    }
    else if (target == 'plugin') {
      obj.addEventListener ('change', function () {
	option['plugin'] = this.value;
	rwMyCookies ('write');
	if (player['playerStatus'] == 'play') playMyVideo(true);
      }, false);
    }
  }
  else if (event == 'click') {
    obj.addEventListener ('click', function () {
      if (action == 'close') {
	removeMyElement(page.body, target);
      }
      else if (action == 'play') {
	playMyVideo(true);
      }
      else if (action == 'get') {
	getMyVideo();
      }
      else if (action == 'autoplay') {
	option['autoplay'] = (option['autoplay']) ? false : true;
	if (option['autoplay']) {
	  styleMyElement (player['buttonPlay'], '', '', '', '', '', '1px solid #CCCCCC', '3px', '', '0px 5px', 'none', '', '#37B704', '10px', '', '0px 1px 1px #CCCCCC', 'pointer', '');
	  styleMyElement (player['buttonAutoplay'], '', '', '', '', '', '1px solid #CCCCCC', '3px', '0px 0px 0px 20px', '0px 5px', 'inline', '', '#008080', '10px', '', '0px 1px 1px #CCCCCC', 'pointer', '');
	  if (player['playerStatus'] == 'thumbnail') playMyVideo(true);
	}
	else {
	  styleMyElement (player['buttonPlay'], '', '', '', '', '', '1px solid #CCCCCC', '3px', '', '0px 5px', 'inline', '', '#37B704', '10px', '', '0px 1px 1px #CCCCCC', 'pointer', '');
	  styleMyElement (player['buttonAutoplay'], '', '', '', '', '', '1px solid #CCCCCC', '3px', '0px 0px 0px 20px', '0px 5px', 'inline', '', '#CCCCCC', '10px', '', '0px 0px 0px #CCCCCC', 'pointer', '');
	  playMyVideo(false);
	}
	rwMyCookies ('write');
      }
      else if (action == 'definition') {
	if (option['definition'] == 'LD') option['definition'] = 'SD';
	else if (option['definition'] == 'SD') option['definition'] = 'HD';
	else if (option['definition'] == 'HD') option['definition'] = 'LD';
	modifyMyElement (player['buttonDefinition'], 'div', option['definition'], false);
	rwMyCookies ('write');
	selectMyVideo ();
	if (player['playerStatus'] == 'play') playMyVideo(true);
      }
      else if (action == 'container') {
	if (option['container'] == 'MP4') option['container'] = 'WebM';
	else if (option['container'] == 'WebM') option['container'] = 'Any';
	else if (option['container'] == 'Any') option['container'] = 'MP4';
	modifyMyElement (player['buttonContainer'], 'div', option['container'], false);
	rwMyCookies ('write');
	selectMyVideo ();
	if (player['playerStatus'] == 'play') playMyVideo(true);
      }
      else if (action == 'detach') {
	detachMyPlayer();
      }
    }, false);
  }
  return obj;
}

function getMyElement (obj, type, from, value, child, content) {
  var getObj, chObj, coObj;
  var pObj = (!obj) ? page.doc : obj;
  if (type == 'body') getObj = pObj.body;
  else {
    if (from == 'id') getObj = pObj.getElementById(value);
    else if (from == 'class') getObj = pObj.getElementsByClassName(value);
    else if (from == 'tag') getObj = pObj.getElementsByTagName(type);
    else if (from == 'ns') getObj = pObj.getElementsByTagNameNS(value, type);
  }
  chObj = (child >= 0) ? getObj[child] : getObj;
  if (content) {
    if (type == 'body' || type == 'div' || type == 'option') coObj = chObj.innerHTML;
    else if (type == 'object') coObj = chObj.data;
    else if (type == 'img' || type == 'video') coObj = chObj.src;
    else coObj = chObj.textContent;
    return coObj;
  }
  else {
    return chObj;
  }
}

function modifyMyElement (obj, type, content, clear) {
  if (content) {
    if (type == 'div' || type == 'option') obj.innerHTML = content;
    else if (type == 'object') obj.data = content;
    else if (type == 'img' || type == 'video') obj.src = content;
  }
  if (clear) {
    if (obj.hasChildNodes()) {
      while (obj.childNodes.length >= 1) {
        obj.removeChild(obj.firstChild);
      }
    }
  }
}

function styleMyElement (obj, pos, top, left, width, height, border, brRadius, margin, padding, display, bgColor, txtColor, txtSize, txtAlign, txtSh, cursor, zIndex) {
  if (pos) obj.style.position = pos;
  if (top) obj.style.top = top;
  if (left) obj.style.left = left;
  if (width) obj.style.width = width;
  if (height) obj.style.height = height;
  if (border) obj.style.border = border;
  if (brRadius) obj.style.borderRadius = brRadius;
  if (margin) obj.style.margin = margin;
  if (padding) obj.style.padding = padding;
  if (display) obj.style.display = display;
  if (bgColor) obj.style.backgroundColor = bgColor;
  if (txtColor) obj.style.color = txtColor;
  if (txtSize) obj.style.fontSize = txtSize;
  if (txtAlign) obj.style.textAlign = txtAlign;
  if (txtSh) obj.style.textShadow = txtSh;
  if (cursor) obj.style.cursor = cursor;
  if (zIndex) obj.style.zIndex = zIndex;
}

function appendMyElement (parent, child) {
  parent.appendChild(child);
}

function removeMyElement (parent, child) {
  parent.removeChild(child);
}

function replaceMyElement (parent, orphan, child) {
  parent.replaceChild(orphan, child);
}

function createMyPlayer () {
  /* Read My Cookies */
  rwMyCookies ('read');

  /* Add To Player */
  player['panelHeight'] = 18;
  player['panelPadding'] = 2;
  player['playerSize'] = 'normal';

  /* The Panel */
  var panelWidth = player['playerWidth'] - player['panelPadding'] * 2;
  panelWidth = panelWidth + 'px';
  var panelHeight = player['panelHeight'];
  panelHeight = panelHeight + 'px';
  panelPadding = player['panelPadding'] + 'px';
  player['playerPanel'] = createMyElement ('div', '', '', '', '');
  styleMyElement (player['playerPanel'], '', '', '', panelWidth, panelHeight, '', '', '', panelPadding, '', '#F4F4F4', '#666666', '10px', 'center', '', '', '');
  appendMyElement (player['playerWindow'], player['playerPanel']);

  /* Panel Items */
  var panelItemBorder = 1;
  var panelItemHeight = player['panelHeight'] - panelItemBorder * 2;
  panelItemHeight = panelItemHeight + 'px';
  
  /* Panel Logo */
  player['panelLogo'] = createMyElement ('div', 'ViewTube:', '', '', '');
  styleMyElement (player['panelLogo'], '', '', '', '', panelItemHeight, '1px solid #F4F4F4', '3px', '', '0px', 'inline', '#F4F4F4', '#336699', '10px', '', '0px 1px 1px #CCCCCC', '', '');
  appendMyElement (player['playerPanel'], player['panelLogo']);

  /* Panel Video Menu */
  player['videoMenu'] = createMyElement ('select', '', 'change', '', 'video');
  styleMyElement (player['videoMenu'], '', '', '', '200px', panelItemHeight, '1px solid #F4F4F4', '3px', '', '0px', 'inline', '#F4F4F4', '#336699', '10px', '', '0px 1px 1px #CCCCCC', 'pointer', '');
  player['videoMenu'].style.verticalAlign = 'baseline';
  appendMyElement (player['playerPanel'], player['videoMenu'] );
  for (var videoCode in player['videoList']) {
    player['videoItem'] = createMyElement ('option', videoCode, '', '', '');
    styleMyElement (player['videoItem'], '', '', '', '', '', '', '', '', '0px', 'block', '#F4F4F4', '#336699', '10px', '', '0px 1px 1px #CCCCCC', 'pointer', '');
    appendMyElement (player['videoMenu'], player['videoItem']);
  }

  /* Panel Plugin Menu */
  player['pluginMenu'] = createMyElement ('select', '', 'change', '', 'plugin');
  styleMyElement (player['pluginMenu'], '', '', '', '70px', panelItemHeight, '1px solid #F4F4F4', '3px', '', '0px', 'inline', '#F4F4F4', '#336699', '10px', '', '0px 1px 1px #CCCCCC', 'pointer', '');
  player['pluginMenu'].style.verticalAlign = 'baseline';
  appendMyElement (player['playerPanel'], player['pluginMenu'] );
  for (var p = 0; p < plugins.length; p++) {
    player['pluginItem'] = createMyElement ('option', plugins[p], '', '', '');
    styleMyElement (player['pluginItem'], '', '', '', '', '', '', '', '', '0px', 'block', '#F4F4F4', '#336699', '10px', '', '0px 1px 1px #CCCCCC', 'pointer', '');
    appendMyElement (player['pluginMenu'], player['pluginItem']);
  }
  player['pluginMenu'].value = option['plugin'];
    
  /* Panel Play Button */
  player['buttonPlay'] = createMyElement ('div', 'Play »', 'click', 'play', '');
  if (option['autoplay']) styleMyElement (player['buttonPlay'], '', '', '', '', panelItemHeight, '1px solid #CCCCCC', '3px', '', '0px 5px', 'none', '', '#37B704', '10px', '', '0px 1px 1px #CCCCCC', 'pointer', '');
  else styleMyElement (player['buttonPlay'], '', '', '', '', '', '1px solid #CCCCCC', '3px', '', '0px 5px', 'inline', '', '#37B704', '10px', '', '0px 1px 1px #CCCCCC', 'pointer', '');
  appendMyElement (player['playerPanel'], player['buttonPlay']);
  
  /* Panel Get Button */
  player['buttonGet'] = createMyElement ('div', 'Get »', 'click', 'get', '');
  styleMyElement (player['buttonGet'], '', '', '', '', panelItemHeight, '1px solid #CCCCCC', '3px', '', '0px 5px', 'inline', '', '#C000C0', '10px', '', '0px 1px 1px #CCCCCC', 'pointer', '');
  appendMyElement (player['playerPanel'], player['buttonGet']);

  /* Panel Features */
  var bMargin;
  var bMarginLeft = 0;
  if (player['playerWidth'] > 600) bMarginLeft = 20;
  else if (player['playerWidth'] > 700) bMarginLeft = 50;
  bMargin = '0px 0px 0px ' + bMarginLeft + 'px';
  
  /* Panel Autoplay Button */
  player['buttonAutoplay'] = createMyElement ('div', 'Autoplay', 'click', 'autoplay', '');
  if (option['autoplay']) styleMyElement (player['buttonAutoplay'], '', '', '', '', panelItemHeight, '1px solid #CCCCCC', '3px', bMargin, '0px 5px', 'inline', '', '#008080', '10px', '', '0px 1px 1px #CCCCCC', 'pointer', '');
  else styleMyElement (player['buttonAutoplay'], '', '', '', '', panelItemHeight, '1px solid #CCCCCC', '3px', bMargin, '0px 5px', 'inline', '', '#CCCCCC', '10px', '', '', 'pointer', '');
  appendMyElement (player['playerPanel'], player['buttonAutoplay']);

  /* Panel Definition Button */
  if (feature['definition']) {
    player['buttonDefinition'] = createMyElement ('div', option['definition'], 'click', 'definition', '');
    styleMyElement (player['buttonDefinition'], '', '', '', '', panelItemHeight, '1px solid #CCCCCC', '3px', '', '0px 5px', 'inline', '', '#008000', '10px', '', '0px 1px 1px #CCCCCC', 'pointer', '');
    appendMyElement (player['playerPanel'], player['buttonDefinition']);
  }

  /* Panel Container Button */
  if (feature['container']) {
    player['buttonContainer'] = createMyElement ('div', option['container'], 'click', 'container', '');
    styleMyElement (player['buttonContainer'], '', '', '', '', panelItemHeight, '1px solid #CCCCCC', '3px', '', '0px 5px', 'inline', '', '#008000', '10px', '', '0px 1px 1px #CCCCCC', 'pointer', '');
    appendMyElement (player['playerPanel'], player['buttonContainer']);
  }

  /* Panel Detach Button */
  if (feature['fullsize']) {
    player['buttonDetach'] = createMyElement ('div', '+', 'click', 'detach', '');
    styleMyElement (player['buttonDetach'], '', '', '', '', panelItemHeight, '1px solid #CCCCCC', '3px', '', '0px 5px', 'inline', '', '#C05800', '10px', '', '1px 1px 2px #CCCCCC', 'pointer', '');
    appendMyElement (player['playerPanel'], player['buttonDetach']);
  }

  /* The Content */
  player['contentWidth'] = player['playerWidth'];
  player['contentHeight'] = player['playerHeight'] - player['panelHeight'] - player['panelPadding'] * 2;
  var contentWidth = player['contentWidth'] + 'px';
  var contentHeight = player['contentHeight'] + 'px';
  player['playerContent'] = createMyElement ('div', '', '', '', '');
  styleMyElement (player['playerContent'], '', '', '', contentWidth, contentHeight, '', '', '', '', '', '#F4F4F4', '#AD0000', '14px', 'center', '', '', '');
  appendMyElement (player['playerWindow'], player['playerContent']);
  
  /* The Video Thumbnail */
  if (player['videoThumb']) {
    player['contentImage'] = createMyElement ('img', player['videoThumb'], '', '', '');
    styleMyElement (player['contentImage'], '', '', '', contentWidth, contentHeight, '0px', '', '', '', '', '', '', '', '', '', '', '');
  }

  /* Select & Play The Video */
  if (feature['definition'] || feature['container']) selectMyVideo ();
  playMyVideo (option['autoplay']);
}

function selectMyVideo () {
  if (feature['container'] && option['container'] != 'Any') {
    var vdoCont = [option['container']];
  }
  else {
    var vdoCont = ['MP4', 'WebM', 'FLV', 'MOV', 'M4V'];
  }
  var vdoDef = ['Full High Definition', 'High Definition', 'Standard Definition', 'Low Definition', 'Very Low Definition'];
  var vdoList = {};
  for (var vC = 0; vC <= vdoCont.length; vC++) {
    for (var vD = 0; vD <= vdoDef.length; vD++) {
      var format = vdoDef[vD] + ' ' + vdoCont[vC];
      if (!vdoList[vdoDef[vD]]) {
	for (var vL in player['videoList']) {
	  if (vL.match(format)) {
	    vdoList[vdoDef[vD]] = vL;
	    break;
	  }
	}
      }
    }
  }
  if (option['definition'] == 'HD') {
    if (vdoList['Full High Definition']) player['videoPlay'] = vdoList['Full High Definition'];
    else if (vdoList['High Definition']) player['videoPlay'] = vdoList['High Definition'];
    else if (vdoList['Standard Definition']) player['videoPlay'] = vdoList['Standard Definition'];
    else if (vdoList['Low Definition']) player['videoPlay'] = vdoList['Low Definition'];
  }
  else if (option['definition'] == 'SD') {
    if (vdoList['Standard Definition']) player['videoPlay'] = vdoList['Standard Definition'];
    else if (vdoList['Low Definition']) player['videoPlay'] = vdoList['Low Definition'];
  }
  else if (option['definition'] == 'LD') {
    if (vdoList['Very Low Definition']) player['videoPlay'] = vdoList['Very Low Definition'];
    else if (vdoList['Low Definition']) player['videoPlay'] = vdoList['Low Definition'];
    else if (vdoList['Standard Definition']) player['videoPlay'] = vdoList['Standard Definition'];
  }
  player['videoMenu'].value = player['videoPlay'];
}

function playMyVideo (play) {
  if (play) {
    modifyMyElement (player['playerContent'], 'div', '', true);
    if (player['playerStatus'] != 'play') {
      player['playerStatus'] = 'play';
      modifyMyElement (player['buttonPlay'], 'div', 'Playing', false);
    }
    if (option['plugin'] == 'HTML5') player['contentVideo'] = createMyElement ('video', player['videoList'][player['videoPlay']], '', '', '');
    else player['contentVideo'] = createMyElement ('object', player['videoList'][player['videoPlay']], '', '', '');
    var contentWidth = player['contentWidth'] + 'px';
    var contentHeight = player['contentHeight'] + 'px';
    styleMyElement (player['contentVideo'], '', '', '', contentWidth, contentHeight, '', '', '', '', '', '', '', '', '', '', '', '');
    appendMyElement (player['playerContent'], player['contentVideo']);
  }
  else {
    if (!option['autoplay'] && player['playerStatus'] == 'play') modifyMyElement (player['buttonPlay'], 'div', 'Play »', false);
    if (player['playerStatus'] != 'thumbnail') {
      player['playerStatus'] = 'thumbnail';
      modifyMyElement (player['playerContent'], 'div', '', true);
      if (player['contentImage']) appendMyElement (player['playerContent'], player['contentImage']);
      else showMyMessage ('!thumb');
    }
  }
}

function getMyVideo () {
  var vdoURL = player['videoList'][player['videoPlay']];
  var vdoD = ' (' + player['videoPlay'] + ')';
  vdoD = vdoD.replace(/High Definition/, 'HD');
  vdoD = vdoD.replace(/Standard Definition/, 'SD');
  vdoD = vdoD.replace(/Low Definition/, 'LD');
  vdoD = vdoD.replace(/\sFLV|\sMP4|\sWebM/g, '');
  if (player['videoTitle']) vdoURL = vdoURL + '&title=' + player['videoTitle'] + vdoD;
  if (option['autoget']) page.win.location.href = vdoURL;
  else {
    var vdoLink = 'Get » <a href="' + vdoURL + '">Link</a>';
    modifyMyElement (player['buttonGet'] , 'div', vdoLink, false);
    player['playerStatus'] = 'download';
  }
}

function detachMyPlayer () {
  if (player['playerSize'] == 'normal') {
    modifyMyElement (player['buttonDetach'], 'div', '-', false);
    removeMyElement (player['playerSocket'], player['playerWindow']);
    appendMyElement (page.body, player['playerWindow']);
    player['playerSize'] = 'large';
    var playerWidth = page.win.innerWidth;
    var playerHeight = page.win.innerHeight;
    var playerPosition = 'fixed';
  }
  else {
    modifyMyElement (player['buttonDetach'], 'div', '+', false);
    removeMyElement (page.body, player['playerWindow']);
    appendMyElement (player['playerSocket'], player['playerWindow']);
    player['playerSize'] = 'normal';
    var playerWidth = player['playerWidth'];
    var playerHeight = player['playerHeight'];
    var playerPosition = 'relative';
  }
    
  var panelWidth = playerWidth - player['panelPadding'] * 2;
  var panelHeight = player['panelHeight'];
  player['contentWidth'] = playerWidth;
  player['contentHeight'] = playerHeight - player['panelHeight'];
    
  /* Resize The Player */
  playerWidth = playerWidth + 'px';
  playerHeight = playerHeight + 'px';
  styleMyElement (player['playerWindow'], playerPosition, '0px', '0px', playerWidth, playerHeight, '', '', '', '', '', '', '', '', '', '', '', '');

  /* Resize The Panel */
  panelWidth = panelWidth + 'px';
  panelHeight = panelHeight + 'px';
  panelPadding = player['panelPadding'] + 'px';
  styleMyElement (player['playerPanel'], '', '', '', panelWidth, panelHeight, '', '', '', panelPadding, '', '', '', '', '', '', '', '');

  /* Resize The Content */
  var contentWidth = player['contentWidth'] + 'px';
  var contentHeight = player['contentHeight'] + 'px';
  styleMyElement (player['playerContent'], '', '', '', contentWidth, contentHeight, '', '', '', '', '', '', '', '', '', '', '', '');
  if (player['contentImage']) styleMyElement (player['contentImage'], '', '', '', contentWidth, contentHeight, '0px', '', '', '', '', '', '', '', '', '', '', '');
  if (player['playerStatus'] == 'play') playMyVideo(true);
}

function cleanMyContent (content, unesc) {
  var myNewContent = content;
  if (unesc) myNewContent = unescape (myNewContent);
  myNewContent = myNewContent.replace (/\\u0025/g,'%');
  myNewContent = myNewContent.replace (/\\u0026/g,'&');
  myNewContent = myNewContent.replace (/\\/g,'');
  myNewContent = myNewContent.replace (/\n/g,'');
  return myNewContent;
}

function getMyContent (url, pattern, clean) {
  var myPageContent, myVideosParse, myVideosContent;
  var retry = false;
  if (url == page.url) {
    myPageContent = getMyElement ('', 'body', '', '', -1, true);
    if (clean) myPageContent = cleanMyContent (myPageContent, true);
    myVideosParse = myPageContent.match (pattern);
    myVideosContent = (myVideosParse != null) ? myVideosParse[1] : null;
    if (myVideosContent != null) return myVideosContent;
    else retry = true;
  }
  if (url != page.url || retry) {
    var xmlHTTP = new XMLHttpRequest();
    xmlHTTP.open('GET', url, false);
    xmlHTTP.send();
    if (pattern == '_XML') {
      myVideosContent = xmlHTTP.responseXML;
    }
    else if (pattern == '_TEXT') {
      myVideosContent = xmlHTTP.responseText;
    }
    else {
      myPageContent = xmlHTTP.responseText;
      if (clean) myPageContent = cleanMyContent (myPageContent, true);
      myVideosParse = myPageContent.match (pattern);
      myVideosContent = (myVideosParse != null) ? myVideosParse[1] : null;
    }
    return myVideosContent;
  }
}

function rwMyCookies (readorwrite) {
  if (readorwrite == 'read') {
    var vtPlugin = 'viewtube_plugin=';
    var vtAutoplay = 'viewtube_autoplay=';
    var vtDefinition = 'viewtube_definition=';
    var vtContainer = 'viewtube_container=';
    var cookies = page.doc.cookie.split(';');
    for(var i=0; i < cookies.length; i++) {
      var cookie = cookies[i];
      while (cookie.charAt(0) == ' ') cookie = cookie.substring(1, cookie.length);
      if (cookie.indexOf(vtPlugin) == 0) {
	option['plugin'] = cookie.substring(vtPlugin.length, cookie.length);
      }
      if (cookie.indexOf(vtAutoplay) == 0) {
	option['autoplay'] = cookie.substring(vtAutoplay.length, cookie.length);
	option['autoplay'] = (option['autoplay'] == 'true') ? true : false;
      }
      if (cookie.indexOf(vtDefinition) == 0) {
	option['definition'] = cookie.substring(vtDefinition.length, cookie.length);
      }
      if (cookie.indexOf(vtContainer) == 0) {
	option['container'] = cookie.substring(vtContainer.length, cookie.length);
      }
    }
  }
  else if (readorwrite == 'write') {
    var date = new Date();
    date.setTime(date.getTime() + (356*24*60*60*1000));
    var expires = '; expires=' + date.toGMTString();
    page.doc.cookie = 'viewtube_plugin=' + option['plugin'] + expires + '; path=/';
    page.doc.cookie = 'viewtube_autoplay=' + option['autoplay'] + expires + '; path=/';
    page.doc.cookie = 'viewtube_definition=' + option['definition'] + expires + '; path=/';
    page.doc.cookie = 'viewtube_container=' + option['container'] + expires + '; path=/';
  }
}

function showMyMessage (mess) {
  var myScriptName = 'ViewTube';
  var myScriptLogo = createMyElement ('div', myScriptName, '', '', '');
  styleMyElement (myScriptLogo, '', '', '', '', '', '', '', '0px auto', '10px', '', '', '#666666', '24px', 'center', '#FFFFFF -1px -1px 2px', '', '');
  var myScriptMess = createMyElement ('div', '', '', '', '');
  styleMyElement (myScriptMess, '', '', '', '', '', '1px solid #F4F4F4', '', '5px auto 5px auto', '10px', '', '#FFFFFF', '#AD0000', '', 'center', '', '', '');
  if (mess == '!player') {
    var myScriptAlert = createMyElement ('div', '', '', '', '');
    styleMyElement (myScriptAlert, 'absolute', '30%', '35%', '', '', '1px solid #F4F4F4', '3px', '', '10px', '', '#F8F8F8', '', '14px', 'center', '', '', 99999);
    appendMyElement (myScriptAlert, myScriptLogo);
    var myNoPlayerMess = 'Couldn\'t get the player element. Please report it <a href="' + contact + '">here</a>.';
    modifyMyElement (myScriptMess, 'div', myNoPlayerMess, false);
    appendMyElement (myScriptAlert, myScriptMess);
    var myScriptAlertButton = createMyElement ('div', 'OK', 'click', 'close', myScriptAlert);
    styleMyElement (myScriptAlertButton, '', '', '', '100px', '', '3px solid #EEEEEE', '5px', '0px auto', '', '', '#EEEEEE', '#666666', '18px', '', '#FFFFFF -1px -1px 2px', 'pointer', '');
    appendMyElement (myScriptAlert, myScriptAlertButton);
    appendMyElement (page.body, myScriptAlert);
  }
  else if (mess == '!thumb') {
    var myNoThumbMess = '<br><br>Couldn\'t get the thumbnail for this video. Please report it <a href="' + contact + '">here</a>.';
    modifyMyElement (myPlayerContent, 'div', myNoThumbMess, false);
  }
  else {
    var myNoContentMess = 'Couldn\'t get the videos content. Please report it <a href="' + contact + '">here</a>.';
    var myNoVideosMess = 'Couldn\'t get any video. Please report it <a href="' + contact + '">here</a>.';
    appendMyElement (myPlayerWindow, myScriptLogo);
    if (mess == '!content') modifyMyElement (myScriptMess, 'div', myNoContentMess, false);
    else if (mess == '!videos') modifyMyElement (myScriptMess, 'div', myNoVideosMess, false);
    appendMyElement (myPlayerWindow, myScriptMess);
  }
}


// ==========Websites========== //

// =====YouTube===== //

if (page.url.indexOf('youtube.com/watch') != -1) {

  /* Check Video Availability */
  if (getMyElement ('', 'div', 'id', 'watch-player-unavailable', -1, false)) return;
 
  /* YouTube Main Function */
  function youtube() {
    
    /* Hide Flash Messages */
    var ytFlashMess = getMyElement ('', 'div', 'id', 'flash10-promo-div', -1, false);
    if (ytFlashMess != null) {
      styleMyElement (ytFlashMess, '', '', '', '0px', '0px', '0px', '', '', '0px', '', '#FFFFFF', '', '', '', '', '', '');
    }

    /* Get Player Window */
    var ytPlayerWindow = getMyElement ('', 'div', 'id', 'watch-video', -1, false);
    if (ytPlayerWindow == null) {
      showMyMessage ('!player');
    }
    else {
      /* My Player Window */
      myPlayerWindow = createMyElement ('div', '', '', '', '');
      styleMyElement (myPlayerWindow, '', '', '', '640px', '390px', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '99999');
      modifyMyElement (ytPlayerWindow, 'div', '', true);
      appendMyElement (ytPlayerWindow, myPlayerWindow);

      /* Get Video Thumbnail */
      var ytVideoThumb = getMyContent (page.url, 'meta.*?property="og:image".*?content="(.*?)"', false);
      if (!ytVideoThumb) ytVideoThumb = getMyContent (page.url, 'link.*?itemprop="thumbnailUrl".*?href="(.*?)"', false);

      /* Get Video Title */
      var ytVideoTitle = getMyContent (page.url, 'meta.*?property="og:title".*?content="(.*?)"', false);
      if (ytVideoTitle) {
	ytVideoTitle = ytVideoTitle.replace(/&quot;/g, '\'').replace(/&#34;/g, '\'').replace(/"/g, '\'');
	ytVideoTitle = ytVideoTitle.replace(/&#39;/g, '\'').replace(/'/g, '\'');
	ytVideoTitle = ytVideoTitle.replace(/&amp;/g, 'and').replace(/&/g, 'and');
	ytVideoTitle = ytVideoTitle.replace(/\?/g, '').replace(/[#:\*]/g, '-').replace(/\//g, '-');
	ytVideoTitle = ytVideoTitle.replace(/^\s+|\s+$/, '').replace(/\.+$/g, '');
      }

      /* Get Videos Content */
      var ytVideosContent = getMyContent (page.url, '"url_encoded_fmt_stream_map": "(.*?)"', false);

      /* Get Videos */
      if (ytVideosContent != null) {
	ytVideosContent = cleanMyContent (ytVideosContent, false);
	var ytVideoFormats = {'5': 'Very Low Definition FLV', '18': 'Low Definition MP4', '22': 'High Definition MP4', '34': 'Low Definition FLV', '35': 'Standard Definition FLV', '37': 'Full High Definition MP4', '43': 'Low Definition WebM', '44': 'Standard Definition WebM', '45': 'High Definition WebM', '46': 'Full High Definition WebM', '82': 'Low Definition 3D MP4', '83': 'Standard Definition 3D MP4', '84': 'High Definition 3D MP4', '85': 'Full High Definition 3D MP4', '100': 'Low Definition 3D WebM', '101': 'Standard Definition 3D WebM', '102': 'High Definition 3D WebM'};
	var ytVideoList = {};
	var ytVideoFound = false;
	var ytVideos = ytVideosContent.split(',');
	var ytVideoParse, ytVideoCodeParse, ytVideoCode, myVideoCode, ytVideo;
	for (var i = 0; i < ytVideos.length; i++) {
	  if (ytVideos[i].match(/^itag/)) {
	    ytVideoParse = ytVideos[i].match(/(^itag=.*)&(url=.*$)/);
	    if (ytVideoParse) ytVideos[i] = ytVideoParse[2] + '&' + ytVideoParse[1];
	  }
	  ytVideoCodeParse = ytVideos[i].match (/itag=(.*?)$/);
	  ytVideoCode = (ytVideoCodeParse != null) ? ytVideoCodeParse[1] : null;
	  if (ytVideoCode != null) {
	    myVideoCode = ytVideoFormats[ytVideoCode];
	    if (myVideoCode != null) {
	      ytVideo = ytVideos[i].replace (/url=/, '');
	      ytVideo = ytVideo.replace (/&quality.*$/, '').replace(/&type.*$/, '');
	      ytVideo = cleanMyContent (ytVideo, true);
	      if (ytVideo) {
		if (!ytVideoFound) ytVideoFound = true;
		ytVideoList[myVideoCode] = ytVideo;
	      }
	    }
	  }
	}
	  
	if (ytVideoFound) {
	  /* Create Player */
	  var ytDefaultVideo = 'Low Definition MP4';
	  player = {'playerSocket': ytPlayerWindow, 'playerWindow': myPlayerWindow, 'videoList': ytVideoList, 'videoPlay': ytDefaultVideo, 'videoThumb': ytVideoThumb, 'videoTitle': ytVideoTitle, 'playerWidth': 640, 'playerHeight': 390};
	  option['autoget'] = true;
	  feature['container'] = true;
	  createMyPlayer ();
	}
	else {
	  showMyMessage ('!videos');
	}
      }
      else {
	showMyMessage ('!content');
      }
    }
  
  }
  
  /* Remove HTML5 Videos */
  var tryagain = true;
  var trywait = 500;
  function rmH5Videos() {
    var ytH5Video = getMyElement ('', 'video', 'tag', '', 0, false);
    var ytH5Object = getMyElement ('', 'object', 'tag', '', 0, false);
    if (ytH5Video || ytH5Object) {
      if (ytH5Video) {
	ytH5Video.addEventListener('play', function() {
	  modifyMyElement (ytH5Video, 'video', 'novideoplease', true);
	  youtube();
	},false);
      }
      if (ytH5Object) {
	removeMyElement (ytH5Object.parentNode, ytH5Object);
	youtube();
      }
    }
    else {
      if (tryagain) {
	tryagain = false;
	setTimeout(rmH5Videos, trywait);
      }
      else {
	youtube();
      }
    }
  }
  rmH5Videos();

}

// =====DailyMotion===== //

else if (page.url.indexOf('dailymotion.com/video') != -1) {

  /* Get Player Window */
  var dmPlayerWindow = getMyElement ('', 'div', 'class', 'dmpi_video_playerv4 span-8', 0, false);
  if (dmPlayerWindow == null) {
    showMyMessage ('!player');
  }
  else {
    /* My Player Window */
    modifyMyElement (dmPlayerWindow, 'div', '', true);
    myPlayerWindow = createMyElement ('div', '', '', '', '');
    styleMyElement (myPlayerWindow, '', '', '', '100%', '100%', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '99999');
    appendMyElement (dmPlayerWindow, myPlayerWindow);

    /* Get Video Thumbnail */
    var dmVideoThumb = getMyContent (page.url, 'meta.*?property="og:image".*?content="(.*?)"', false);

    /* Get Videos Content */
    var dmVideosContent = getMyContent (page.url, '"VideoFrame","param":\{(.*?)\}', true);

    /* Get Videos */
    if (dmVideosContent != null) {
      var dmVideoFormats = {'hd1080URL': 'Full High Definition MP4', 'hd720URL': 'High Definition MP4', 'hqURL': 'Standard Definition MP4', 'sdURL': 'Low Definition MP4'};
      var dmVideoList = {};
      var dmVideoFound = false;
      var dmVideoParser, dmVideoParse, myVideoCode, dmVideo;
      for (var dmVideoCode in dmVideoFormats) {
	dmVideoParser = '"' + dmVideoCode + '":"(.*?)"';
	dmVideoParse = dmVideosContent.match (dmVideoParser);
	dmVideo = (dmVideoParse != null) ? dmVideoParse[1] : null;
	if (dmVideo) {
	  if (!dmVideoFound) dmVideoFound = true;
	  myVideoCode = dmVideoFormats[dmVideoCode];
	  dmVideoList[myVideoCode] = dmVideo;
	}
      }

      if (dmVideoFound) {
	/* Create Player */
	var dmDefaultVideo = 'Low Definition MP4';
	player = {'playerSocket': dmPlayerWindow, 'playerWindow': myPlayerWindow, 'videoList': dmVideoList, 'videoPlay': dmDefaultVideo, 'videoThumb': dmVideoThumb, 'playerWidth': 620, 'playerHeight': 350};
	createMyPlayer ();
      }
      else {
	showMyMessage ('!videos');
      }
    }
    else {
      showMyMessage ('!content');
    }
  }
  
}

// =====MetaCafe===== //

else if (page.url.indexOf('metacafe.com/watch') != -1) {

  /* Get Player Window */
  var mcPlayerWindow = getMyElement ('', 'div', 'id', 'adaptvDiv', -1, false);
  if (mcPlayerWindow == null) {
    showMyMessage ('!player');
  }
  else {
    /* My Player Window */
    modifyMyElement (mcPlayerWindow, 'div', '', true);
    myPlayerWindow = createMyElement ('div', '', '', '', '');
    styleMyElement (myPlayerWindow, '', '', '', '100%', '100%', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '99999');
    appendMyElement (mcPlayerWindow, myPlayerWindow);

    /* Get Video Thumbnail */
    var mcVideoThumb = getMyContent (page.url, 'meta.*?property="og:image".*?content="(.*?)"', false);

    /* Get Videos Content */
    var mcVideosContent = getMyContent (page.url, '"mediaData":"\{(.*?)\}"', true);

    /* Get Videos */
    if (mcVideosContent != null) {
      var mcVideoFormats = {'highDefinitionMP4': 'High Definition MP4', 'MP4': 'Low Definition MP4'};
      var mcVideoList = {};
      var mcVideoFound = false;
      var mcVideoParser, mcVideoParse, myVideoCode, mcVideoPath, mcVideoKey, mcVideo;
      for (var mcVideoCode in mcVideoFormats) {
	mcVideoParser = '"' + mcVideoCode + '":.*?"mediaURL":"(.*?)","key":"(.*?)"';
	mcVideoParse = mcVideosContent.match (mcVideoParser);
	mcVideoPath = (mcVideoParse != null) ? mcVideoParse[1] : null;
	mcVideoKey = (mcVideoParse != null) ? mcVideoParse[2] : null;
	if (mcVideoPath && mcVideoKey) {
	  if (!mcVideoFound) mcVideoFound = true;
	  myVideoCode = mcVideoFormats[mcVideoCode];
	  mcVideo = mcVideoPath + '?__gda__=' + mcVideoKey;
	  mcVideoList[myVideoCode] = mcVideo;
	}
      }

      if (mcVideoFound) {
	/* Create Player */
	var mcDefaultVideo = 'Low Definition MP4';
	player = {'playerSocket': mcPlayerWindow, 'playerWindow': myPlayerWindow, 'videoList': mcVideoList, 'videoPlay': mcDefaultVideo, 'videoThumb': mcVideoThumb, 'playerWidth': 615, 'playerHeight': 380};
	createMyPlayer ();
      }
      else {
	showMyMessage ('!videos');
      }
    }
    else {
      showMyMessage ('!content');
    }
  }
  
}

// =====Vimeo===== //

else if (page.url.match(/vimeo.com($|\/$|\/\d{1,8})/) != null) {

  /* Get Player Window */
  var viPlayerWindow = getMyElement ('', 'div', 'class', 'vimeo_holder', 0, false) || null;
  if (viPlayerWindow == null) {
    showMyMessage ('!player');
  }
  else {
    /* My Player Window */    
    modifyMyElement (viPlayerWindow, 'div', '', true);
    myPlayerWindow = createMyElement ('div', '', '', '', '');
    styleMyElement (myPlayerWindow, '', '', '', '100%', '100%', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '99999');
    appendMyElement (viPlayerWindow, myPlayerWindow);

    /* Get Videos Content */
    var viVideosContent = getMyContent (page.url, 'config:\{(.*?)\}\};', false);

    /* Get Videos */
    if (viVideosContent != null) {
      var viVideoFormats = {'hd': 'High Definition MP4', 'sd': 'Low Definition MP4', 'mobile': 'Very Low Definition MP4'};
      var viVideoList = {};
      var viVideoFound = false;
      var viVideoSignature, viVideoTimestamp,  viVideoID, viVideoQualities, viVideoThumb, viVideo, myVideoCode;
      viVideoSignature = viVideosContent.match (/"signature":"(.*?)"/);
      viVideoSignature = (viVideoSignature) ? viVideoSignature[1] : null;
      viVideoTimestamp = viVideosContent.match (/"timestamp":(.*?),/);
      viVideoTimestamp = (viVideoTimestamp) ? viVideoTimestamp[1] : null;
      viVideoPlayer = viVideosContent.match (/"player_url":"(.*?)"/);
      viVideoPlayer = (viVideoPlayer) ? viVideoPlayer[1] : null;
      viVideoID = viVideosContent.match (/"video":{"id":(.*?),/);
      viVideoID = (viVideoID) ? viVideoID[1] : null;
      viVideoQualities = viVideosContent.match (/"qualities":\[(.*?)\]/);
      viVideoQualities = (viVideoQualities) ? viVideoQualities[1] : null;
      viVideoThumb = viVideosContent.match (/"thumbnail":"(.*?)"/);
      viVideoThumb = (viVideoThumb) ? cleanMyContent(viVideoThumb[1]) : null;
      if (viVideoSignature && viVideoTimestamp && viVideoID && viVideoPlayer && viVideoQualities) {
	for (var viVideoCode in viVideoFormats) {
	  if (viVideoQualities.indexOf(viVideoCode) != -1) {
	    if (!viVideoFound) viVideoFound = true;
	    viVideo = 'http://' + viVideoPlayer + '/play_redirect?' + 'quality=' + viVideoCode + '&clip_id=' + viVideoID + '&time=' + viVideoTimestamp + '&sig=' + viVideoSignature;
	    myVideoCode = viVideoFormats[viVideoCode];
	    viVideoList[myVideoCode] = viVideo;
	  }
	}
      }

      if (viVideoFound) {
	/* Create Player */
	var viDefaultVideo = 'Standard Definition MP4';
	player = {'playerSocket': viPlayerWindow, 'playerWindow': myPlayerWindow, 'videoList': viVideoList, 'videoPlay': viDefaultVideo, 'videoThumb': viVideoThumb, 'playerWidth': 960, 'playerHeight': 540};
	createMyPlayer ();
      }
      else {
	showMyMessage ('!videos');
      }
    }
    else {
      showMyMessage ('!content');
    } 
  }
  
}

// =====Break===== //

else if (page.url.indexOf('break.com') != -1) {

  /* Get Page Type */
  var brPageType = getMyContent (page.url, 'meta.*?property="og:type".*?content="(.*?)"', false);
  if (!brPageType || brPageType != 'video.other') return;

  /* Get Player Window */
  var brPlayerWindow = getMyElement ('', 'div', 'id', 'playerwrap', -1, false);
  if (brPlayerWindow == null) {
    showMyMessage ('!player');
  }
  else {
    /* My Player Window */    
    modifyMyElement (brPlayerWindow, 'div', '', true);
    styleMyElement (brPlayerWindow, '', '', '', '580px', '380px', '', '', '', '', '', '', '', '', '', '', '', '');
    myPlayerWindow = createMyElement ('div', '', '', '', '');
    styleMyElement (myPlayerWindow, '', '', '', '100%', '100%', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '99999');
    appendMyElement (brPlayerWindow, myPlayerWindow);
  
    /* Get Video Thumbnail */
    var brVideoThumb = getMyContent (page.url, 'meta.*?property="og:image".*?content="(.*?)"', false);
    
    /* Get Videos Content */
    var brVideosContent = getMyContent (page.url, '(sGlobalFileName=.*?)isUGC', false);

    /* Check For Token */
    if (brVideosContent && brVideosContent.indexOf('sGlobalToken') == -1) {
      var brVideosToken = getMyContent (page.url, 'flashVars.icon.*?=.*?"(.*?)"', false);
      brVideosContent += ';sGlobalToken=\'' + brVideosToken + '\';';
    }

    /* Get Videos */
    if (brVideosContent != null) {
      var brVideoFormats = {'sGlobalFileNameHDD': 'High Definition MP4',  'sGlobalFileNameHD': 'Standard Definition MP4', 'sGlobalFileName': 'Low Definition MP4'};
      var brVideoList = {};
      var brVideoFound = false;
      var brVideoParser, brVideoParse, myVideoCode, brVideo, brVideoPath, brVideoGlue;
      var brTokenParse = brVideosContent.match (/sGlobalToken=.(.*?).;/);
      var brToken = (brTokenParse != null) ? brTokenParse[1] : null;
      for (var brVideoCode in brVideoFormats) {
	brVideoParser = brVideoCode + '=.(.*?).;';
	brVideoParse = brVideosContent.match (brVideoParser);
	brVideo = (brVideoParse != null) ? brVideoParse[1] : null;
	if (brVideo && brToken) {
	  if (!brVideoFound) brVideoFound = true;
	  myVideoCode = brVideoFormats[brVideoCode];
	  brVideoGlue = '?';
	  if (myVideoCode == 'Low Definition MP4') {
	    if (brVideo.indexOf('.flv') != -1 || brVideo.indexOf('.mp4') != -1) {
	      if (brVideo.indexOf('.flv') != -1) myVideoCode = 'Low Definition FLV';
	    }
	    else {
	      brVideoGlue = '.mp4?';
	    }
	  }
	  brVideoPath = brVideo + brVideoGlue + brToken;
	  brVideoList[myVideoCode] = brVideoPath;
	}
      }
      if (brVideosContent.indexOf('isEmbed=true') != -1 || brVideosContent.indexOf('isEmbed="true"') != -1 || brVideosContent.indexOf('isEmbed=\'true\'') != -1) brVideoFound = false;
 
      if (brVideoFound) {
	/* Create Player */
	var brDefaultVideo = 'Low Definition MP4';
	player = {'playerSocket': brPlayerWindow, 'playerWindow': myPlayerWindow, 'videoList': brVideoList, 'videoPlay': brDefaultVideo, 'videoThumb': brVideoThumb, 'playerWidth': 580, 'playerHeight': 380};
	createMyPlayer ();
      }
      else {
	showMyMessage ('!videos');
      }
    }
    else {
      showMyMessage ('!content');
    }
  }
  
}

// =====FunnyOrDie===== //

else if (page.url.indexOf('funnyordie.com/videos') != -1) {
  
  /* Get Player Window */
  var fodPlayerWindow = getMyElement ('', 'div', 'id', 'video_player', -1, false);
  if (fodPlayerWindow == null) {
    showMyMessage ('!player');
  }
  else {
    /* My Player Window */
    modifyMyElement (fodPlayerWindow, 'div', '', true);
    myPlayerWindow = createMyElement ('div', '', '', '', '');
    styleMyElement (myPlayerWindow, '', '', '', '100%', '100%', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '99999');
    appendMyElement (fodPlayerWindow, myPlayerWindow);

    /* Get Video Thumbnail */
    var fodVideoThumb = getMyContent (page.url, 'meta.*?property="og:image".*?content="(.*?)"', false);
    if (fodVideoThumb != null) fodVideoThumb = fodVideoThumb.replace (/large/, 'fullsize');

    /* Get Videos Content */
    var fodVideosContent = getMyContent (page.url, '(video_tag =.*mp4)', true);

    /* Get Videos */
    if (fodVideosContent != null) {
      var fodVideoFormats = {'ipad': 'High Definition MP4', 'iphone_wifi': 'Low Definition MP4'};
      var fodVideoList = {};
      var fodVideoFound = false;
      var fodVideoParser, myVideoCode, fodVideoParse, fodVideo;
      var fodVideos = fodVideosContent.match (/http.*?(\s|$)/g);
      for (var fodVideoCode in fodVideoFormats) {
	for (var i = 0; i < fodVideos.length; i++) {
	  fodVideoParser = 'http.*?' + fodVideoCode + '.mp4';
	  fodVideo = fodVideos[i].match (fodVideoParser);
	  if (fodVideo) {
	    if (!fodVideoFound) fodVideoFound = true;
	    myVideoCode = fodVideoFormats[fodVideoCode];
	    fodVideoList[myVideoCode] = fodVideo;
	    break;
	  }
	}
      }

      if (fodVideoFound) {
	/* Create Player */
	fodDefaultVideo = 'Low Definition MP4';
	player = {'playerSocket': fodPlayerWindow, 'playerWindow': myPlayerWindow, 'videoList': fodVideoList, 'videoPlay': fodDefaultVideo, 'videoThumb': fodVideoThumb, 'playerWidth': 640, 'playerHeight': 460};
	createMyPlayer ();
      }
      else {
	showMyMessage ('!videos');
      }
    }
    else {
      showMyMessage ('!content');
    } 
  }
  
}

// =====Blip.TV===== //

else if (page.url.indexOf('blip.tv') != -1) {

  /* Get Page Type */
  var blipPageType = getMyContent (page.url, 'meta.*?property="og:type".*?content="(.*?)"', false);
  if (!blipPageType || blipPageType != 'bliptvviewer:web_series_episode') return;

  /* Get Player Window */
  var blipPlayerWindow = getMyElement ('', 'div', 'id', 'PlayerEmbed', -1, false);
  if (blipPlayerWindow == null) {
    showMyMessage ('!player');
  }
  else {
    /* My Player Window */    
    modifyMyElement (blipPlayerWindow, 'div', '', true);
    myPlayerWindow = createMyElement ('div', '', '', '', '');
    styleMyElement (myPlayerWindow, '', '', '', '100%', '100%', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '99999');
    appendMyElement (blipPlayerWindow, myPlayerWindow);

    /* Get Video Thumbnail */
    var blipVideoThumb = getMyContent (page.url, 'meta.*?property="og:image".*?content="(.*?)"', false);
    
    /* Get Video ID */
    var blipVideoID = page.url.match (/(\d{7})$/)[1];

    /* Get Videos Content */
    var blipVideosContent = getMyContent ('http://blip.tv/rss/flash/' + blipVideoID, '_XML', false);

    if (blipVideosContent != null) {
      /* Get Videos */
      var blipVideos = getMyElement (blipVideosContent, 'content', 'ns', 'http://search.yahoo.com/mrss/', -1, false);

      /* Get Videos */
      if (blipVideos.length > 0) {
	var blipVideoList = {};
	var blipVideoFound = false;
	var blipMimeTypes = {'video/x-m4v': 'M4V', 'video/mp4': 'MP4', 'video/quicktime': 'MOV', 'video/x-flv': 'FLV'};
	var blipVideo, blipVideoHight, blipVideoDef, blipVideoType, blipDefault, blipVideoCont, blipVideoCode, blipDefaultVideo;
	for (var i = 0; i < blipVideos.length; i++) {
	  blipVideo = blipVideos[i].getAttribute('url');
	  blipVideoHeight = blipVideos[i].getAttribute('height');
	  blipVideoDef = 'Unknown Definition';
	  if (blipVideoHeight >= 200 && blipVideoHeight < 400) blipVideoDef = 'Low Definition';
	  else if (blipVideoHeight >= 400 && blipVideoHeight < 700) blipVideoDef = 'Standard Definition';
	  else if (blipVideoHeight >= 700) blipVideoDef = 'High Definition';
	  blipVideoType = blipVideos[i].getAttribute('type');
	  blipDefault = blipVideos[i].getAttribute('isDefault');
	  blipVideoCont = blipMimeTypes[blipVideoType];
	  if (blipVideo && blipVideoType && blipVideoCont) {
	    if (!blipVideoFound) blipVideoFound = true;
	    blipVideoCode = blipVideoDef + ' ' + blipVideoCont;
	    if (blipDefault == 'true') blipDefaultVideo = blipVideoCode;
	    blipVideoList[blipVideoCode] = blipVideo;
	  }
	}

	if (blipVideoFound) {
	  /* Create Player */
	  player = {'playerSocket': blipPlayerWindow, 'playerWindow': myPlayerWindow, 'videoList': blipVideoList, 'videoPlay': blipDefaultVideo, 'videoThumb': blipVideoThumb, 'playerWidth': 780, 'playerHeight': 450};
	  createMyPlayer ();
	}
	else {
	  showMyMessage ('!videos');
	}
      }
    }
    else {
      showMyMessage ('!content');
    }
  }
  
}

// =====Veoh===== //

else if (page.url.indexOf('veoh.com/watch') != -1) {

  /* Get Player Window */
  var vePlayerWindowContainer = getMyElement ('', 'div', 'id', 'videoContainer', -1, false);
  var vePlayerWindow = getMyElement ('', 'div', 'id', 'videoPlayerContainer', -1, false);

  if (vePlayerWindowContainer == null || vePlayerWindow == null) {
    showMyMessage ('!player');
  }
  else {
    /* My Player Window */
    myPlayerWindow = createMyElement ('div', '', '', '', '');
    styleMyElement (myPlayerWindow, '', '', '', '640px', '400px', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '99999');
    replaceMyElement (vePlayerWindowContainer, myPlayerWindow, vePlayerWindow);

    /* Get Videos Content */
    var veVideosContent = getMyContent (page.url, '__watch.videoDetailsJSON = \'\{(.*?)\}', false);
    veVideosContent = cleanMyContent (veVideosContent, true);
    
    /* Get Video Thumbnail */
    var veVideoThumbGet = veVideosContent.match (/"highResImage":"(.*?)"/);
    var veVideoThumb = (veVideoThumbGet != null) ? veVideoThumbGet[1] : null;
      
    /* Get Videos */
    if (veVideosContent != null) {
      var veVideoFormats = {'previewUrl': 'Low Definition FLV', 'ipodUrl': 'Low Definition MP4'};
      var veVideoList = {};
      var veVideoFound = false;
      var veVideoParser, veVideoParse, veVideo, myVideoCode;
      for (var veVideoCode in veVideoFormats) {
	veVideoParser = veVideoCode + '":"(.*?)"';
	veVideoParse = veVideosContent.match (veVideoParser);
	veVideo = (veVideoParse != null) ? veVideoParse[1] : null;
	if (veVideo) {
	  if (!veVideoFound) veVideoFound = true;
	  myVideoCode = veVideoFormats[veVideoCode];
	  veVideoList[myVideoCode] = veVideo;
	}
      }

      if (veVideoFound) {
	/* Create Player */
	var veDefaultVideo = 'Low Definition FLV';
	player = {'playerSocket': vePlayerWindow, 'playerWindow': myPlayerWindow, 'videoList': veVideoList, 'videoPlay': veDefaultVideo, 'videoThumb': veVideoThumb, 'playerWidth': 640, 'playerHeight': 400};
	feature['fullsize'] = false;
	createMyPlayer ();
      }
      else {
	showMyMessage ('!videos');
      }
    }
    else {
      showMyMessage ('!content');
    }
  }
  
}

// =====VeeHD===== //

else if (page.url.indexOf('veehd.com/video/') != -1) {

  /* Get Player Window */
  var veePlayerWindow = getMyElement ('', 'div', 'class', 'videoHolder', 0, false);
  if (veePlayerWindow == null) {
    showMyMessage ('!player');
  }
  else {
    /* Get Video Thumb */
    var veeVideoThumb = getMyContent (page.url, 'img.*?id="veehdpreview".*?src="(.*?)"', false);
    
    /* Get Videos Content */
    var veeVideosContent = getMyContent (page.url, '"(\/vpi\\?h=.*?)"', false);
    
    /* My Player Window */ 
    modifyMyElement (veePlayerWindow, 'div', '', true);
    styleMyElement (veePlayerWindow, '', '', '', '990px', '', '', '', '', '', '', '', '', '', '', '', '', '');
    myPlayerWindow = createMyElement ('div', '', '', '', '');
    styleMyElement (myPlayerWindow, '', '', '', '100%', '100%', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '99999');
    appendMyElement (veePlayerWindow, myPlayerWindow);

    /* Get Videos */
    if (veeVideosContent) {
      var veeVideoList = {};
      var veeVideoFound = false;
      var veeVideoType = getMyContent (page.url, 'type:\\s(.*?)<', false);
      var veeVideo, veeVideoContainer, veeVideoSize, veeVideoDefinition, veeDefaultVideo;
      if (veeVideoType == 'divx') {
	veeVideo = getMyContent (veeVideosContent, 'param.*?name="src".*?value="(.*?)"', true);
	veeVideoContainer = 'AVI';
      }
      else {
	veeVideo = getMyContent (veeVideosContent, '"url":"(.*?)"', true);
	veeVideoContainer = 'MP4';
      }
      veeVideoSize = getMyContent (page.url, 'resolution:.*?x(.*?)<', false);
      veeVideoDefinition = 'Low Definition';
      if (veeVideoSize > 400) veeVideoDefinition = 'Standard Definition';
      if (veeVideoSize > 700) veeVideoDefinition = 'High Definition';
      if (veeVideoSize > 1000) veeVideoDefinition = 'Full High Definition';
      veeDefaultVideo = veeVideoDefinition + ' ' + veeVideoContainer;
      if (veeVideo) {
	if (!veeVideoFound) veeVideoFound = true;
	veeVideoList[veeDefaultVideo] = veeVideo;
      }
      
      if (veeVideoFound) {
	/* Create Player */
	player = {'playerSocket': veePlayerWindow, 'playerWindow': myPlayerWindow, 'videoList': veeVideoList, 'videoPlay': veeDefaultVideo, 'videoThumb': veeVideoThumb, 'playerWidth': 990, 'playerHeight': 480};
	feature['definition'] = false;	
	createMyPlayer ();
      }
      else {
	showMyMessage ('!videos');
      }
    }
    else {
      showMyMessage ('!content');
    }
  }
  
}

// =====Mevio===== //

else if (page.url.indexOf('mevio.com') != -1) {
    
  /* Get & Remove Player Zone */
  var mePlayerZone = getMyElement ('', 'div', 'id', 'player-zone', -1, false);
  if (mePlayerZone != null) removeMyElement(mePlayerZone.parentNode, mePlayerZone);

  if (page.url.indexOf('mevio.com/episode') != -1) {
    
    /* Get Player Window */
    var mePlayerWindowContainer = getMyElement ('', 'div', 'class', 'section-header', 0, false);

    if (mePlayerWindowContainer == null) {
      showMyMessage ('!player');
    }
    else {
      
      /* My Player Socket */
      mePlayerWindow = createMyElement ('div', '', '', '', '');
      styleMyElement (mePlayerWindow, '', '', '', '580px', '360px', '', '', '5px 0px', '', '', '#FFFFFF', '', '', '', '', '', '');
      appendMyElement(mePlayerWindowContainer, mePlayerWindow);
      
      /* My Player Window */
      myPlayerWindow = createMyElement ('div', '', '', '', '');
      styleMyElement (myPlayerWindow, '', '', '', '580px', '360px', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '99999');
      appendMyElement(mePlayerWindow, myPlayerWindow);

      /* Get Data Content */
      var meDataContent = getMyContent (page.url, 'args.default_media.*?=.*?\{(.*?)\};', false);
      meDataContent = cleanMyContent (meDataContent, true);

      /* Get Video Thumbnail */
      var meVideoThumb = meDataContent.match(/"large":"(.*?)"/);
      meVideoThumb = (meVideoThumb) ? meVideoThumb[1] : null;

      /* Get Videos Content */
      var meVideosContent = meDataContent.match(/"media_urls":\{(.*?)\}/);
      meVideosContent = (meVideosContent) ? meVideosContent[1] : null;

      /* Get Videos */
      if (meVideosContent != null) {
	var meVideoFormats = {'mp4': 'High Definition MP4', 'm4v': 'High Definition M4V',  'mov': 'High Definition MOV', 'h264': 'Low Definition MP4', 'flv': 'Low Definition FLV'};
	var meVideoList = {};
	var meVideoFound = false;
	var meVideoParser, meVideoParse, meVideo, myVideoCode;
	for (var meVideoCode in meVideoFormats) {
	  meVideoParser = meVideoCode + '":"(.*?)"';
	  meVideoParse = meVideosContent.match (meVideoParser);
	  meVideo = (meVideoParse != null) ? meVideoParse[1] : null;
	  if (meVideo) {
	    if (!meVideoFound) meVideoFound = true;
	    myVideoCode = meVideoFormats[meVideoCode];
	    meVideoList[myVideoCode] = meVideo;
	  }
	}

	if (meVideoFound) {
	  /* Create Player */
	  var meDefaultVideo = 'Low Definition FLV';
	  player = {'playerSocket': mePlayerWindow, 'playerWindow': myPlayerWindow, 'videoList': meVideoList, 'videoPlay': meDefaultVideo, 'videoThumb': meVideoThumb, 'playerWidth': 580, 'playerHeight': 360};
	  createMyPlayer ();
	}
	else {
	  showMyMessage ('!videos');
	}
      }
      else {
	showMyMessage ('!content');
      }

    }
    
  }
  
}

// =====5min===== //

else if (page.url.indexOf('5min.com/Video') != -1) {

  /* Get Player Window */
  var fmPlayerWindow = getMyElement ('', 'div', 'class', 'player', 0, false);

  if (fmPlayerWindow == null) {
    showMyMessage ('!player');
  }
  else {
    /* Get Videos Content */
    var fmVideosContent = getMyElement ('', 'object', 'id', 'fiveMinPlayer', -1, true);
    fmVideosContent = cleanMyContent (fmVideosContent, true);
    
    /* My Player Window */
    modifyMyElement (fmPlayerWindow, 'div', '', true);
    myPlayerWindow = createMyElement ('div', '', '', '', '');
    styleMyElement (myPlayerWindow, '', '', '', '655px', '398px', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '99999');
    appendMyElement (fmPlayerWindow, myPlayerWindow);
      
    /* Get Videos */
    if (fmVideosContent != null) {
      var fmGetVideo = fmVideosContent.match(/videoUrl=(.*?)\&/);
      var fmVideo = (fmGetVideo) ? fmGetVideo[1] : null;
      var fmGetThumb = fmVideosContent.match(/previewPic=(.*?)\&/);
      var fmVideoThumb = (fmGetThumb) ? fmGetThumb[1] : null;
      var fmVideoList = {};
      var fmDefaultVideo;
      if (fmVideo.match(/mp4$/)) {
	var fmGetRendition = fmVideosContent.match(/defaultRendition=(.*?)\&/);
	var fmRendition = (fmGetRendition) ? fmGetRendition[1] : null;
	fmVideo = fmVideo.replace(/.mp4/, '_' + fmRendition + '.mp4');
	fmDefaultVideo = 'Standard Definition MP4';
      }
      else if (fmVideo.match(/flv$/)) {
	fmDefaultVideo = 'Standard Definition FLV';
      }
      
      if (fmVideo) {
	/* Create Player */
	fmVideoList[fmDefaultVideo] = fmVideo;
	player = {'playerSocket': fmPlayerWindow, 'playerWindow': myPlayerWindow, 'videoList': fmVideoList, 'videoPlay': fmDefaultVideo, 'videoThumb': fmVideoThumb, 'playerWidth': 655, 'playerHeight': 398};
	feature['definition'] = false;
	createMyPlayer ();
      }
      else {
	showMyMessage ('!videos');
      }
    }
    else {
      showMyMessage ('!content');
    }
  }
  
}

// =====Videojug===== //

else if (page.url.indexOf('videojug.com/film') != -1) {

  /* Get Player Window */
  var vjPlayerWindow = getMyElement ('', 'div', 'id', 'player', -1, false);

  if (vjPlayerWindow == null) {
    showMyMessage ('!player');
  }
  else {
    /* My Player Window */
    modifyMyElement (vjPlayerWindow, 'div', '', true);
    styleMyElement (vjPlayerWindow, '', '', '', '640px', '380px', '', '', '', '', '', '#FFFFFF', '', '', '', '', '', '');
    myPlayerWindow = createMyElement ('div', '', '', '', '');
    styleMyElement (myPlayerWindow, '', '', '', '640px', '380px', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '99999');
    appendMyElement (vjPlayerWindow, myPlayerWindow);

    /* Get Videos Content */
    var vjVideosContent = getMyContent (page.url, 'new.*?Player\\((.*?)\\)', true);

    /* Get Videos */
    if (vjVideosContent != null) {
      vjVideosContent = vjVideosContent.replace(/'/g, '').replace(/\s/g, '');
      var vjVideosParts = vjVideosContent.split(',');
      var vjVideoToken = vjVideosParts[3];
      var vjVideoToken2 = vjVideoToken.substring(0,2);
      var vjVideoTitle = vjVideosParts[7];
      var vjVideoFormats = {'VJ480PENG.mp4': 'Standard Definition MP4', 'VJ360PENG.mp4': 'Low Definition MP4',  'FW8ENG.flv': 'Low Definition FLV', 'FS8ENG.flv': 'Very Low Definition FLV'};
      var vjVideoList = {};
      var vjVideoFound = false;
      var vjVideoPart, vjVideoHost, myVideoCode, vjVideo, vjVideoThumb;
      if (vjVideoToken && vjVideoTitle) {
	vjVideoFound = true;
	vjVideoPart = vjVideoToken2 + '/' + vjVideoToken + '/' + vjVideoTitle;
	for (var vjVideoCode in vjVideoFormats) {
	  if (vjVideoCode == 'FS8ENG.flv') vjVideoHost = 'http://content.videojug.com/';
	  else vjVideoHost = 'http://content3.videojug.com/';
	  vjVideo =  vjVideoHost + vjVideoPart + '__' + vjVideoCode;
	  myVideoCode = vjVideoFormats[vjVideoCode];
	  vjVideoList[myVideoCode] = vjVideo;
	}
	vjVideoHost = 'http://content5.videojug.com/';
	vjVideoThumb =  vjVideoHost + vjVideoPart + '.WidePlayer.jpg';
      }
      
      if (vjVideoFound) {
	/* Create Player */
	var vjDefaultVideo = 'Low Definition MP4';
	player = {'playerSocket': vjPlayerWindow, 'playerWindow': myPlayerWindow, 'videoList': vjVideoList, 'videoPlay': vjDefaultVideo, 'videoThumb': vjVideoThumb, 'playerWidth': 640, 'playerHeight': 380};
	createMyPlayer ();
      }
      else {
	showMyMessage ('!videos');
      }
    }
    else {
      showMyMessage ('!content');
    }
  }
  
}

// =====Facebook===== //

else if (page.url.indexOf('facebook.com/video/video') != -1 || page.url.indexOf('facebook.com/photo.php?v=') != -1) {

  /* Get Player Window */
  var fbPlayerWindow = getMyElement ('', 'div', 'class', 'stageWrapper', 0, false);
  if (fbPlayerWindow == null) {
    showMyMessage ('!player');
  }
  else {
    /* My Player Window */ 
    modifyMyElement (fbPlayerWindow, 'div', '', true);
    myPlayerWindow = createMyElement ('div', '', '', '', '');
    styleMyElement (myPlayerWindow, '', '', '', '100%', '100%', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '99999');
    appendMyElement (fbPlayerWindow, myPlayerWindow);

    /* Get Video Thumbnail */
    var fbVideoThumb = getMyContent (page.url, '"thumb_url",.*?"(.*?)"', true);
    fbVideoThumb = cleanMyContent (fbVideoThumb, true);

    /* Get Videos Content */
    var fbVideosContent = getMyContent (page.url, 'JSCC.init\\(\\((.*?)\\)\\)', true);

    /* Get Videos */
    if (fbVideosContent != null) {
      var fbVideoFormats = {};
      var fbVideoList = {};
      var fbVideoFound = false;
      var fbVideoHQ = (fbVideosContent.indexOf ('highqual_src') != -1) ? true : false;
      if (fbVideoHQ) {
	var fbVideoHDGet = fbVideosContent.match (/"video_has_high_def",.*?"(.*?)"/);
	var fbVideoHD = (fbVideoHDGet != null) ? fbVideoHDGet[1] : false;
	if (fbVideoHD) fbVideoFormats = {'highqual_src': 'High Definition MP4', 'lowqual_src': 'Low Definition MP4'};
	else fbVideoFormats = {'highqual_src': 'Standard Definition MP4', 'lowqual_src': 'Low Definition MP4'};
      }
      else {
	fbVideoFormats = {'video_src': 'Low Definition FLV'};
      }
      var fbVideoParser, fbVideoParse, myVideoCode, fbVideo;
      fbVideosContent = cleanMyContent (fbVideosContent, true);
      for (var fbVideoCode in fbVideoFormats) {
	fbVideoParser = fbVideoCode + '",.*?"(.*?)"';
	fbVideoParse = fbVideosContent.match (fbVideoParser);
	fbVideo = (fbVideoParse != null) ? fbVideoParse[1] : null;
	if (fbVideo) {
	  if (!fbVideoFound) fbVideoFound = true;
	  myVideoCode = fbVideoFormats[fbVideoCode];
	  fbVideoList[myVideoCode] = fbVideo;
	}
      }

      if (fbVideoFound) {
	/* Create Player */
	var fbDefaultVideo = (fbVideoHD || fbVideoHQ) ? 'Low Definition MP4' : 'Low Definition FLV';
	player = {'playerSocket': fbPlayerWindow, 'playerWindow': myPlayerWindow, 'videoList': fbVideoList, 'videoPlay': fbDefaultVideo, 'videoThumb': fbVideoThumb, 'playerWidth': 716, 'playerHeight': (fbVideoHD || fbVideoHQ) ? 450 : 485};
	createMyPlayer ();
      }
      else {
	showMyMessage ('!videos');
      }
    }
    else {
      showMyMessage ('!content');
    }
  }
  
}

// =====IMDB===== //

else if (page.url.indexOf('imdb.com/video/') != -1) {

  /* Get Player Window */
  var imdbPlayerWindow = getMyElement ('', 'div', 'id', 'player-article', -1, false);
  if (imdbPlayerWindow == null) {
    showMyMessage ('!player');
  }
  else {
    /* My Player Window */ 
    modifyMyElement (imdbPlayerWindow, 'div', '', true);
    styleMyElement (imdbPlayerWindow, '', '', '', '640px', '480px', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '');
    myPlayerWindow = createMyElement ('div', '', '', '', '');
    styleMyElement (myPlayerWindow, '', '', '', '100%', '100%', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '99999');
    appendMyElement (imdbPlayerWindow, myPlayerWindow);

    /* Get Videos Content */
    var imdbVideoList = {};
    var imdbVideoFormats = {'1': 'Low Definition MP4', '2': 'Standard Definition MP4', '3': 'High Definition MP4'};
    var imdbVideoThumb, imdbDefaultVideo, imdbURL, imdbVideo, myVideoCode;
    var imdbVideoFound = false;
    for (var imdbVideoCode in imdbVideoFormats) {
      imdbURL = page.url + 'player?uff=' + imdbVideoCode;
      imdbVideo = getMyContent (imdbURL, 'so.addVariable\\("file", "(.*?)"\\);', true);
      if (imdbVideoThumb == null) {
	imdbVideoThumb = getMyContent (imdbURL, 'so.addVariable\\("image", "(.*?)"\\);', true);
      }
      if (imdbVideo && imdbVideo.match (/rtmp/) == null) {
	if (!imdbVideoFound) imdbVideoFound = true;
	myVideoCode = imdbVideoFormats[imdbVideoCode];
	imdbVideoList[myVideoCode] = imdbVideo;
	if (!imdbDefaultVideo) imdbDefaultVideo = myVideoCode;
      }
    }

    if (imdbVideoFound) {
      /* Create Player */
      player = {'playerSocket': imdbPlayerWindow, 'playerWindow': myPlayerWindow, 'videoList': imdbVideoList, 'videoPlay': imdbDefaultVideo, 'videoThumb': imdbVideoThumb, 'playerWidth': 640, 'playerHeight': 480};
      //feature['fullsize'] = false;
      createMyPlayer ();
    }
    else {
      showMyMessage ('!videos');
    }
  }
  
}

// =====Discovery===== //

else if (page.url.indexOf('discovery.com/videos/') != -1) {

  /* Get Player Window */
  var dscPlayerWindow = getMyElement ('', 'div', 'id', 'video-player-container', -1, false);
  if (dscPlayerWindow == null) {
    showMyMessage ('!player');
  }
  else {
    /* My Player Window */ 
    modifyMyElement (dscPlayerWindow, 'div', '', true);
    if (page.url.indexOf('.html') != -1) var dscPlayerWidth = 662;
    else dscPlayerWidth = 620;
    var dscPlayerWidthPx = dscPlayerWidth + 'px';
    styleMyElement (dscPlayerWindow, '', '', '', dscPlayerWidthPx, '390px', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '');
    myPlayerWindow = createMyElement ('div', '', '', '', '');
    styleMyElement (myPlayerWindow, '', '', '', '100%', '100%', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '99999');
    appendMyElement (dscPlayerWindow, myPlayerWindow);

    /* Get Videos Content */
    var dscVideosContent = getMyContent (page.url, 'clipJSON = \{(.*?)}', false);
    
    /* Get Videos */
    if (dscVideosContent) {
      var dscVideoList = {};
      var dscVideoFound = false;
      var dscVideoFormats = {'1500k': 'Standard Definition FLV', '800k': 'Low Definition FLV', '200k': 'Very Low Definition FLV'};
      var dscVideoBase = dscVideosContent.match(/"m3u8":"(.*?),.*?"/);
      dscVideoBase = (dscVideoBase) ? dscVideoBase[1] : null;
      dscVideoBase = dscVideoBase.replace(/\/i\//, '/');
      var dscVideoThumb = dscVideosContent.match(/"thumbnailURL":"(.*?)"/);
      dscVideoThumb = (dscVideoThumb) ? dscVideoThumb[1] : null;
      var dscVideo, myVideoCode, dscDefaultVideo;
      for (var dscVideoCode in dscVideoFormats) {
	if (dscVideoBase) {
	  if (!dscVideoFound) dscVideoFound = true;
	  dscVideo = dscVideoBase + dscVideoCode + '.mp4';
	  myVideoCode = dscVideoFormats[dscVideoCode];
	  dscVideoList[myVideoCode] = dscVideo;
	  if (!dscDefaultVideo) dscDefaultVideo = myVideoCode;
	}
      }
      
      if (dscVideoFound) {
	/* Create Player */
	player = {'playerSocket': dscPlayerWindow, 'playerWindow': myPlayerWindow, 'videoList': dscVideoList, 'videoPlay': dscDefaultVideo, 'videoThumb': dscVideoThumb, 'playerWidth': dscPlayerWidth, 'playerHeight': 390};
	createMyPlayer ();
      }
      else {
	showMyMessage ('!videos');
      }
    }
    else {
      showMyMessage ('!content');
    }
  }
  
}

// =====NationalGeographic===== //

else if (page.url.indexOf('nationalgeographic.com/video/') != -1) {

  /* Remove Top Ad */
  var ngAdsBoard = getMyElement ('', 'div', 'id', 'headerboard', -1, false);
  if (ngAdsBoard) removeMyElement(ngAdsBoard.parentNode, ngAdsBoard);
  
  /* Get Player Window */
  var ngPlayerWindow = getMyElement ('', 'div', 'id', 'natgeov-vid-outer', -1, false);
  if (ngPlayerWindow == null) {
    showMyMessage ('!player');
  }
  else {
    /* My Player Window */ 
    modifyMyElement (ngPlayerWindow, 'div', '', true);
    styleMyElement (ngPlayerWindow, '', '', '', '610px', '375px', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '');
    myPlayerWindow = createMyElement ('div', '', '', '', '');
    styleMyElement (myPlayerWindow, '', '', '', '100%', '100%', '', '', '', '', '', '#F4F4F4', '', '', '', '', '', '99999');
    appendMyElement (ngPlayerWindow, myPlayerWindow);

    /* Get Video Name */
    var ngVideoLink = getMyContent (page.url, 'meta.*?property="og:video".*?content="(.*?)"', false);
    var ngVideoParts = ngVideoLink.split('/');
    var ngVideoName = ngVideoParts[ngVideoParts.length-2];
    
    /* Get Videos Content */
    var ngVideosContent;
    if (ngVideoName) ngVideosContent = getMyContent ('http://video.nationalgeographic.com/video/player/data/xml/' + ngVideoName + '.smil', '_TEXT', false);
    
    /* Get Video Thumb */
    var ngVideoThumb = getMyContent (page.url, 'meta.*?property="og:image".*?content="(.*?)"', false);
    if (ngVideoThumb) {
      if (ngVideoThumb.indexOf('130/73') != -1) ngVideoThumb = ngVideoThumb.replace(/130\/73/, '610/375');
      else if (ngVideoThumb.indexOf('130x73') != -1) ngVideoThumb = ngVideoThumb.replace(/130x73/, '610x375');
    }

    /* Get Videos */
    if (ngVideosContent) {
      var ngVideoList = {};
      var ngVideoFound = false;
      var ngDefaultVideo;
      var ngVideoBase = ngVideosContent.match (/meta.*?name="httpBase".*?content="(.*?)"/);
      ngVideoBase = (ngVideoBase) ? ngVideoBase[1] : null;
      var ngVideoSrc = ngVideosContent.match (/video.*?src="(.*?)"/);
      ngVideoSrc = (ngVideoSrc) ? ngVideoSrc[1] : null;
      if (ngVideoBase && ngVideoSrc) {
	ngVideoList['Low Definition FLV'] = ngVideoBase + ngVideoSrc;
	ngVideoFound = true;
	ngDefaultVideo = 'Low Definition FLV';
      }

      if (ngVideoFound) {
	/* Create Player */
	player = {'playerSocket': ngPlayerWindow, 'playerWindow': myPlayerWindow, 'videoList': ngVideoList, 'videoPlay': ngDefaultVideo, 'videoThumb': ngVideoThumb, 'playerWidth': 610, 'playerHeight': 375};
	feature['definition'] = false;
	createMyPlayer ();
      }
      else {
	showMyMessage ('!videos');
      }
    }
    else {
      showMyMessage ('!content');
    }
  }
  
}


})();
