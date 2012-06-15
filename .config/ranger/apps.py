# -*- coding: utf-8 -*-
# Copyright (C) 2009, 2010, 2011  Roman Zimbelmann <romanz@lavabit.com>
# This configuration file is licensed under the same terms as ranger.
# ===================================================================

# In order to add application definitions "on top of" the default ones
# in your ~/.config/ranger/apps.py, you should subclass the class defined
# here like this:

import ranger
from ranger.api.apps import *
from ranger.ext.get_executables import get_executables

from ranger.defaults.apps import CustomApplications as DefaultApps
class CustomApplications(DefaultApps):
	def app_default(self, c):
		"""How to determine the default application?"""
		f = c.file

		if f.basename.lower() == 'makefile' and c.mode == 1:
			made = self.either(c, 'make')
			if made: return made

		if f.extension is not None:

			if f.extension in ('pdf', ):
				c.flags += 'd'
				return self.either(c, 'llpp', 'zathura', 'mupdf', 'apvlv',
						'evince', 'okular', 'epdfview')

                        # CUSTOM
			if f.extension == 'djvu':
				c.flags += 'd'
				return self.either(c,  'zathura', 'evince')

			if f.extension == 'ps':
				c.flags += 'd'
				return self.either(c,  'zathura', 'evince')

                        # CUSTOM
			if f.extension == 'm2ts':
				c.flags += 'd'
				return self.either(c,  'mplayer2', 'mplayer', 'smplayer', 'vlc')

			if f.extension in ('xml', 'csv'):
				return self.either(c, 'editor')

			if f.extension == 'mid':
				return self.either(c, 'wildmidi')

			if f.extension in ('html', 'htm', 'xhtml') or f.extension == 'swf':
				c.flags += 'd'
				handler = self.either(c,
						'luakit', 'uzbl', 'vimprobable', 'vimprobable2', 'jumanji',
						'firefox', 'seamonkey', 'iceweasel', 'opera',
						'surf', 'midori', 'epiphany', 'konqueror')
				# Only return if some program was found:
				if handler:
					return handler
			if f.extension in ('html', 'htm', 'xhtml'):
				# These browsers can't handle flash, so they're not called above.
				c.flags += 'D'
				return self.either(c, 'elinks', 'links', 'links2', 'lynx', 'w3m')

			if f.extension == 'nes':
				return self.either(c, 'fceux')

			if f.extension in ('swc', 'smc', 'sfc'):
				return self.either(c, 'zsnes')

			if f.extension == 'doc':
				c.flags += 'd'
				return self.either(c, 'abiword', 'libreoffice',
						'soffice', 'ooffice')
			if f.extension in ('odt', 'ods', 'odp', 'odf', 'odg', 'sxc',
					'stc', 'xls', 'xlsx', 'xlt', 'xlw', 'gnm', 'gnumeric'):
				c.flags += 'd'
				return self.either(c, 'gnumeric', 'kspread',
						'libreoffice', 'soffice', 'ooffice')

		if f.mimetype is not None:
			if INTERPRETED_LANGUAGES.match(f.mimetype):
				return self.either(c, 'edit_or_run')

		if f.container:
			return self.either(c, 'aunpack', 'file_roller')

                # CUSTOM
		if f.video or f.audio:
			if f.video:
				c.flags += 'd'
			return self.either(c, 'mplayer2', 'mplayer', 'smplayer', 'vlc',
					'totem')

                # CUSTOM
		if f.image:
			if c.mode in (11, 12, 13, 14):
				return self.either(c, 'set_bg_with_feh')
			else:
				return self.either(c, 'sxiv', 'feh', 'gpicview' , 'eog', 'mirage')

		if f.document or f.filetype.startswith('text') or f.size == 0:
			return self.either(c, 'editor')

		# You can put this at the top of the function and mimeopen will
		# always be used for every file.
		return self.either(c, 'mimeopen')


	# ----------------------------------------- application definitions
	# Note: Trivial application definitions are at the bottom

        # CUSTOM
	@depends_on('feh', 'X')
	def app_feh(self, c):
		c.flags += 'd'
		if c.mode is 0 and len(c.files) is 1 and self.fm.env.cwd:
			# view all files in the cwd
			images = [f.basename for f in self.fm.env.cwd.files if f.image]
			return 'feh', '-Z', '-.', '--start-at', c.file.basename, images
		return 'feh', '-Z', '-.', c

# Forked applications
CustomApplications.generic(
	'luakit', 'uzbl', 'vimprobable', 'vimprobable2', 'jumanji',
	'firefox', 'seamonkey', 'iceweasel', 'opera',
	'surf', 'midori', 'epiphany', 'konqueror',
	'evince', 'zathura', 'apvlv', 'okular', 'epdfview', 'mupdf', 'llpp',
	'eog', 'mirage', 'gimp',
	'libreoffice', 'soffice', 'ooffice', 'gnumeric', 'kspread', 'abiword',
	'gmplayer', 'smplayer', 'vlc',
			flags='d', deps=['X'])

INTERPRETED_LANGUAGES = re.compile(r'''
	^(text|application)/x-(
		haskell|perl|python|ruby|sh
	)$''', re.VERBOSE)
