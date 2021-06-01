/*
 * Inspinia js helpers:
 *
 * correctHeight() - fix the height of main wrapper
 * detectBody() - detect windows size
 * smoothlyMenu() - add smooth fade in/out on navigation show/ide
 *
 */

declare var jQuery: any;

export function correctHeight() {
    var pageWrapper = jQuery('#page-wrapper');
    var navbarHeight = jQuery('nav.navbar-default').height();
    var wrapperHeight = pageWrapper.height();

    if (navbarHeight > wrapperHeight) {
        pageWrapper.css('min-height', navbarHeight + 'px');
    }

    if (navbarHeight <= wrapperHeight) {
        if (navbarHeight < jQuery(window).height()) {
            pageWrapper.css('min-height', jQuery(window).height() + 'px');
        } else {
            pageWrapper.css('min-height', navbarHeight + 'px');
        }
    }

    if (jQuery('body').hasClass('fixed-nav')) {
        if (navbarHeight > wrapperHeight) {
            pageWrapper.css('min-height', navbarHeight + 'px');
        } else {
            pageWrapper.css('min-height', jQuery(window).height() - 60 + 'px');
        }
    }
}

export function detectBody() {
    if (jQuery(document).width() < 769) {
        jQuery('body').addClass('body-small');
    } else {
        jQuery('body').removeClass('body-small');
    }
}

export function detectIboxTools(tag: string) {
    jQuery(tag + ' .collapse-link').on('click', function (e) {
        e.preventDefault();
        var ibox = jQuery(this).closest('div.ibox');
        var button = jQuery(this).find('i');
        var content = ibox.children('.ibox-content');
        content.slideToggle(200);
        button.toggleClass('fa-chevron-up').toggleClass('fa-chevron-down');
        ibox.toggleClass('').toggleClass('border-bottom');
        setTimeout(function () {
            ibox.resize();
            ibox.find('[id^=map-]').resize();
        }, 50);
    });
}

export function smoothlyMenu() {
    if (
        !jQuery('body').hasClass('mini-navbar') ||
        jQuery('body').hasClass('body-small')
    ) {
        // Hide menu in order to smoothly turn on when maximize menu
        jQuery('#side-menu').hide();
        // For smoothly turn on menu
        setTimeout(function () {
            jQuery('#side-menu').fadeIn(400);
        }, 200);
    } else if (jQuery('body').hasClass('fixed-sidebar')) {
        jQuery('#side-menu').hide();
        setTimeout(function () {
            jQuery('#side-menu').fadeIn(400);
        }, 100);
    } else {
        // Remove all inline style from jquery fadeIn function to reset menu state
        jQuery('#side-menu').removeAttr('style');
    }
}
