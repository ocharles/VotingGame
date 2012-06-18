from fabric.api import *
from fabric.api import *

env.hosts = [ 'scheduling@ocharles.org.uk' ]

def deploy():
    dest = "scheduling-game-%s" % (local("git rev-parse HEAD", capture=True)[:6])

    with settings( hide('stdout') ):
        local("cabal clean")
        local("cabal configure")
        local("cabal build")

    run("mkdir -p %s" % (dest))

    put("resources", "%s/" % (dest),
        mirror_local_mode=True)
    put("dist/build/scheduling-game/scheduling-game", "%s/scheduling-game" % (dest),
        mirror_local_mode=True)
    put("dist/build/scheduling-game-sync/scheduling-game-sync", "%s/scheduling-game-sync" % (dest),
        mirror_local_mode=True)

    run("rm -f ~/live")
    run("ln -s ~/%s ~/live" % (dest))
    run("ln -s ~/site_key.txt ~/live/site_key.txt")
    run("ln -s ~/snaplets ~/live/snaplets")

    run("pkill scheduling-game")
