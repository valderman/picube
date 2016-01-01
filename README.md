picube
======
Silly piece of software for info screens, digital photo frames, etc.
Current capabilities:

* Showing the current time
* Showing random images from [Gelbooru](http://gelbooru.com)


Usage
-----
Quickstart:

Make sure that you have [GHC](http://haskell.org/platform) and
[Haste](http://haste-lang.org) installed,
then install [haste-standalone](https://github.com/valderman/haste-standalone)
twice; once using `cabal` (for GHC) and once using `haste-cabal` (for Haste).

Then run the following:

    $ make release
    $ mkdir img
    $ ./app -w img -d img

This will build both the binary and JS parts of the application, create a cache
directory for downloaded images, and start serving your application on
port 8080.
Visit [http://localhost:8080/app.html](http://localhost:8080/app.html) to
access the info screen, and [http://localhost:8080](http://localhost:8080) to
access the configuration screen.

To show images from Gelbooru, you need to create an account and provide the
info screen with that account and its password. By default, the Gelbooru module
will only show images tagged `rating:safe`. Note, however, that this does not
constitute a guarantee that any images will actually be work safe by your
standards.

When running this program on an actual info screen as opposed to your own
computer, you may want to use different network settings.
See `./app --help` for more information on network configuration.


Building for ARM
---------------
Since Haste is currently not officially supported on ARM devices, if you want
to run your program on such a device (for instance, a Raspberry Pi) you will
need to compile the JavaScript part on another device, transfer it to the ARM
device, and building the binary there.

On your non-ARM device:

    $ make release-js
    $ scp app.js your-arm-device:/path/to/picube/app.js

On your ARM device:

    $ make finish-arm

This should give you an ARM binary which you can then run as described above.
