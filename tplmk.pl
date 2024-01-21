#!/usr/bin/perl
use 5.020;
use warnings;

use experimental 'signatures'; # oh well

use List::Util qw(any);
use Path::Tiny qw(path); # uses File::Spec

use Template;

package cache {
	use parent 'Storable';
	my %paths;
	sub new($class,$path) {
		my $self = eval { return Storable::retrieve($path) }
			|| bless {}, $class;
		$paths{$self} = $path;
		return $self;
	}
	sub DESTROY($self) {
		$self->store($paths{$self});
	}
}

sub path2src {
	return {
		src => $_[0],
		dst => path(do {
			local @_ = File::Spec::->splitdir($_[0]);
			$_[-1] =~ s/\.tt2$//;
			@_[1..$#_]
		}),
		date => $_[0] =~ m{(\d{4})/(\d{2})/(\d{2})}
			? "$1-$2-$3"
			: "",
	}
};

# list all sources and potential destinations first: menu (and rss?) will need that
my @src;
path('src')->visit(
	sub { push @src, path2src($_) if /\.tt2$/i },
	{ recurse => 1 }
);

# FIXME: $force will be true if the FILE is older than its dependencies
# while the cached contents might as well be newer (because of earlier dependencies)
# It "works" for now, but needs some redesigning to avoid
sub process_template ($path,$force=0) {
	$path = "$path"; # force stringify even if passed an object
	warn "Requested $path\n";
	state $cache = cache::->new("cache.db"); # cache if already processed once
	# TODO: cache all_src and see who addresses it using Template::Document TRACE_VARS
	# then we can have automatically updated menu
	if (!exists $cache->{$path} or $force) {
		warn "Refreshing cache for $path\n";
		undef $cache->{$path}; # prevent infinite recursion
		my ($ext) = $path =~ /\.(\w+)\.tt2$/;
		my $tpl = Template::->new({
			VARIABLES => {
				process_template => \&process_template,
				all_src => [ grep { $_->{date} } @src ],
				self => $path, # in case you're interested
			},
			EVAL_PERL => 1,
			INCLUDE_PATH => [qw(lib .)],
			-e "lib/$ext.tt2" ? (WRAPPER => "$ext.tt2") : (),
			STRICT => 1,
		});
		my $content;
		$tpl->process($path,{},\$content) or die $tpl->error;
		$cache->{$path} = {
			content => $content,
			stash => $tpl->context->stash->get('global'),
			time => time
		};
	}
	return $cache->{$path};
}

for my $tpl (@ARGV ? map { path2src path $_ } @ARGV : @src) {
	# dependencies are common includes and files near source template (and the template itself)
	my @deps = (glob("lib/*.tt2"), grep { -f } $tpl->{src}->parent->children);
	# if any of them is newer than destination, rerun the template
	if (any { ! -e $tpl->{dst} or -M $_ < -M $tpl->{dst} } @deps) {
		say "$tpl->{dst} doesn't exist or older than dependencies";
		$tpl->{dst}->touchpath; # the rest of the path might not exist, either
		$tpl->{dst}->spew_utf8(process_template($tpl->{src},1)->{content}) ;
	}
	say "$tpl->{src} done";
}
