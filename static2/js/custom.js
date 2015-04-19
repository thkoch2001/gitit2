$(document).ready(function() {
	// moving things around
	$('#logo').remove();
	$('#sidebar').prepend($('#TOC'));
	$('#sidebar').prepend($('.title').clone());
	// fix the sidebar
	$('#gitit-rows').addClass('padded-rows');
});
$(document).scroll(function() {
    var cutoff = $(window).scrollTop();
    var cutoffRange = cutoff + 200;

    // Find current section and highlight nav menu
    var curSec = $.find('.current');
    var curID = $(curSec).attr('id');
    var curNav = $.find('toc-'+curID);
    $(curNav).addClass('active');

    $('#content > h1').removeClass('current');
    $('#content > h1').each(function(){
	    if($(this).offset().top + $(this).height() > cutoff){
		    $(this).addClass('current');
		    return false; // stops the iteration after the first one on screen
	    }
    });
});