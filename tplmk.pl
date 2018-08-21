#!/usr/bin/perl
use 5.020;
use warnings;

use feature 'signatures';
no warnings 'experimental::signatures'; # oh well

use List::Util qw(any);
use Path::Tiny qw(path); # uses File::Spec

use Template;

# list all sources and potential destinations first: menu (and rss?) will need that
my @src;
path('src')->visit(
	sub {
		push @src, {
			src => $_,
			dst => path(do {
				local @_ = File::Spec::->splitdir($_);
				$_[-1] =~ s/\.tt2$/.html/;
				@_[1..$#_]
			}),
		} if /\.tt2$/i
	},
	{ recurse => 1 }
);

sub process_template ($path) {
	$path = "$path"; # force stringify even if passed an object
	state %cache; # cache if processed once; TODO: persistence
	# once we have persistence, cache all_src and see who addresses it using Template::Document TRACE_VARS
	# then we can have automatically updated menu
	if (!exists $cache{$path}) {
		warn "Processing $path\n";
		undef $cache{$path}; # prevent infinite recursion
		my $tpl = Template::->new({
			VARIABLES => {
				process_template => \&process_template,
				all_src => \@src,
				self => $path, # in case you're interested
			},
			EVAL_PERL => 1,
			INCLUDE_PATH => [qw(lib .)],
			WRAPPER => 'page.tt2',
			STRICT => 1,
		});
		my $content;
		$tpl->process($path,{},\$content) or die $tpl->error;
		$cache{$path} = { content => $content, stash => $tpl->context->stash->get('global') };
	}
	return $cache{$path};
}

for my $tpl (@src) {
	# dependencies are common includes and files near source template (and the template itself)
	my @deps = (glob("lib/*.tt2"), grep { -f } $tpl->{src}->parent->children);
	# if any of them is newer than destination, rerun the template
	$tpl->{dst}->spew(process_template($tpl->{src})->{content}) if any { -M $_ < -M $tpl->{dst} } @deps;
}
