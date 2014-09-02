---
title: Neutrino Astrophysics
---

The Basics
---

At first, neutrinos seem like puzzling particles; they interact only by the
weak force, carry almost no mass, and seem insignificant to the grand
scheme of things. On the contrary, they've played a fundamental role in the
evolution of the Universe, and are a uniquely powerful tool in our
understanding of certain astrophysical phenomena. This is precisely because
of their low interaction rates; they can propagate effortlessly through
optically thick material and bring us information that light cannot.

As you may know, there are three types of neutrinos, the electron, muon,
and tau flavors, which correspond to specific leptons. There are a variety
of neutrino sources. The nearest and most accessible is the Sun, which
sends neutrinos to us at a rate of around a billion per square centimeter
per second. Then there are energetic events like supernovae and gamma ray
bursts, which provide short-lived fluxes of neutrinos. There's also this
thing called the cosmic neutrino background, which is analogous to the
cosmic microwave background. Finally, there are atmospheric neutrinos,
which are produced by cosmic ray interactions with the atmosphere.

Neutrino Production
---

In order to discuss neutrino detection, it is beneficial to examine the
largest source of neutrinos in our neighborhood: fusion in the center of
the Sun. The plot below
shows the neutrino flux as a function of its energy as a result of various
steps in hydrogen fusion.

<figure>
  <img class="centerimage" src="../images/solar-neutrino-spectrum.png" width="80%">
  <figcaption>
  Solar neutrino spectrum.  
  Image from [Bahcall, Solar Neutrinos](http://www.sns.ias.edu/~jnb/Papers/Popular/Wiley/paper.pdf)
  </figcaption>
</figure>


The solid lines show neutrino fluxes versus the energy of the produced
neutrinos for various steps in the proton proton chain. In particular, the
lowest-energy neutrinos shown (the spike on the left) are produced in the
first step of the pp chain, while the other reactions, like boron-8 and
He-p fusion, occur in only a small fraction of solar fusion reactions. The
dashed lines show predicted neutrino fluxes from the CNO cycle, which isn't
terribly relevant to the Sun, since its core is too cool for the CNO cycle
to function.

Neutrino Detectors
---

Neutrino detection is a tricky enterprise in the best of cases. Since neutrinos
interact only via the weak interaction, their interactions have fantastically
low cross-sections, which means that detectors end up seeing only a couple of
neutrinos per day in some of the better cases.

The very first neutrino detector is typically called the Homestake or Davis
experiment, and involved ${}^{37}$Cl in the form of perchloroethylene, a common
cleaning chemical. The interaction monitored by the experiment was
$\nu_e+{}^{37}\text{Cl}\rightarrow e^- + {}^{37}\text{Ar}$. For those of you
without a periodic table handy, this is just the conversion of a neutron in
chlorine to a proton in argon. The lowest-energy neutrinos that could
participate in this reaction had energies of around 0.8 MeV. Luckily for
science, the results of the experiment were interesting enough to warrant a new
generation of detectors, based on gallium-71.

Gallium detectors monitor the reaction $\nu_e+{}^{71}\text{Ga}\rightarrow e^- +
{}^{71}\text{Ge}$, but have a much lower threshold energy: just 0.2 MeV, which
is low enough to detect pp chain neutrinos. Oh, and another fun fact: one of
the gallium neutrino detectors contained 60 tons of gallium, at a time when the
world production of gallium was just 10 tons per year! The threshold energies
for these detector types are shown in the figure below. Both of these
experiment types are chemically based: physicists set up the detectors, leave
them alone for a couple of months, then chemically separate out the desired
atoms, and somehow count them one by one. While these can detect solar
neutrinos, this experimental model has some drawbacks: it can't tell you
exactly when an interaction occurred, and it gives little to no information
about the direction or energy of the incoming neutrino. These drawbacks led to
the next generation of neutrino detection: Cerenkov detectors.

The first Cerenkov detectors looked for elastic scattering between neutrinos
and electrons. In such a process, the electron may be accelerated to faster
than the speed of light in the detector medium (water or ice). When this
occurs, Cerenkov radiation is emitted as the electron decelerates. This is the
source of that beautifully toxic blue you see around nuclear waste in
underwater facilities. The basic structure for a Cerenkov detector is a huge
tank of water (or a cubic kilometer of Antarctic ice, in the case of Ice Cube)
for neutrinos to interact with, surrounded by hundreds or thousands of
photomultiplier tubes to detect the Cerenkov radiation. Elastic scattering of
neutrinos and electrons requires fairly high-energy neutrinos (around 7 MeV),
so it can't detect the pp chain neutrinos that gallium detectors can, but it
has the advantages of time resolution and directional resolution, since
Cerenkov radiation is produced in a cone around the accelerated electron. Note
that the neutrinos that scatter with electrons are primarily electron
neutrinos, though small fractions of the interactions can involve the other
flavors.

Based on this idea is a fourth detector, the Sudbury Solar Neutrino Observatory
(SNO). It is a Cerenkov detector with heavy water, which contains a lot of
deuterium, instead of the normal old hydrogen. The beauty of this detector is
that it's sensitive to two reactions: $\nu_e+D\rightarrow e^-+p+p$, which only
detects electron neutrinos, and $\nu+D\rightarrow \nu'+n+p$, which can detect
all three flavors of electrons. This ability will turn out to be very
beneficial in the resolution of the solar neutrino problem.
<figure>
  <img class="centerimage" src="../images/solar-neutrino-spectrum-annotated.png" width="80%">
  <figcaption>
  Modified image from Bahcall, Solar Neutrinos.  
  http://www.sns.ias.edu/~jnb/Papers/Popular/Wiley/paper.pdf 
  </figcaption>
</figure>


The Solar Neutrino Problem
---

The neutrino detectors discussed in the last section are all well and good, but
there's a bit of a problem. It's substantial enough to have earned itself a
catchy name: the solar neutrino problem. See, the theory of electroweak
interactions and astrophysical models of the Sun makes fairly precise
predictions about the fluxes of neutrinos that should be measured by these
various detectors. But when the detectors went to look for these neutrinos,
they found substantially fewer - around a third to a half as many as theorists
predicted. This was true all across the board, from the chemical to the
Cerenkov detectors, and caused physicists quite the headache.

<figure>
  <img class="centerimage" src="../images/solar-neutrino-problem.png" width="80%">
  <figcaption>
  From Bahcall, Solar Neutrinos.
  </figcaption>
</figure>

Initially, it seemed like this was bad news for us. Neutrinos come to us
straight from the center of the sun, as opposed to photons, which take
thousands to tens of thousands of years to bounce their way out of the center,
so it seemed possible that the lack of neutrinos meant that against all odds,
the sun's fusion was dying!

Luckily, SNO came to the rescue. As you can see at the right of the image
above, the electron-neutrino reaction monitored by SNO, like all the other
detectors, finds less than 30% as many neutrinos as expected. But, when we look
at the other reaction, the one insensitive to neutrino flavor, we find around
90% of the expected flux, within uncertainties. So it seems that somehow the
neutrinos produced in the center of the Sun, which are all electron neutrinos,
somehow morph into other types as they travel to us. This phenomenon is called
neutrino oscillations.

Neutrino Oscillations
---

When last we left our heroes, the Sudbury Solar Neutrino Observatory (SNO) had
concluded that while predictions for the total flux of neutrinos at the Earth
were accurate, only about a third of those that reach us are electron
neutrinos. This naturally begs the question: what are the rest of them?
Subsequent experiments, using both astrophysical and accelerator neutrinos,
concluded that they oscillated into muon- and tau-flavored neutrinos. This
process is highly analogous to strangeness oscillations in the neutral kaons
I've written so much about. In essence, neutrinos are produced and interact in
flavor eigenstates, as the electron neutrino $\nu_e$, the muon neutrino
$\nu_\mu$, and the tau neutrino $\nu_\tau$, but propagate through space in mass
eigenstates creatively named $\nu_1$, $\nu_2$, and $\nu_3$. Since these
particles have distinct masses, they also have distinct time evolution, and
based on oscillation frequencies, we can determine the mass differences between
various eigenstates.

Unlike in the kaon system, however, in which the $K^0$ had an equal probability
of being a $K_1$ compared to a $K_2$, it turns out the $\nu_e$ is much more
likely to be a $\nu_1$ than any other mass eigenstate, so in order to describe
the probabilities associated with converting between the two bases, we have to
introduce the concept of mixing angles. For simplicity, let's just look at two
of the three neutrino types in both bases: $\nu_e$ and $\nu_\mu$ for the flavor
eigenstates and $\nu_1$ and $\nu_2$ for the mass eigenstates. We could choose
two constants to represent the components of $\nu_e$ in the two mass
eigenstates, but in order to ensure normalization, we instead use the sine and
cosine of an angle, $\theta_{12}$, and end up with 
$$\nu_e = \cos(\theta_{12})\nu_1 + \sin(\theta_{12})\nu_2$$
$$\nu_\mu = -\sin(\theta_{12})\nu_1 + \cos(\theta_{12})\nu_2$$

We can similarly convert from the mass eigenstates to the flavor eigenstates.
Throwing in a third state complicates matters somewhat: we have to add two new
mixing angles in order to describe the pairwise relationship between the
states, and also have to add what's called a CP violating phase $\delta$.
Overall, the relationship is a little messy:

$
\left(\begin{array}{c}\nu_e\\ \nu_\mu\\ \nu_\tau \end{array}\right)= \left(\begin{array}{ccc}
c_{12}c_{13} & s_{12}c_{13} & s_{13}e^{-i\delta}\\
-s_{12}c_{23}-c_{12}s_{23}s_{13}e^{i\delta} &
c_{12}c_{23}-s_{12}s_{23}s_{13}e^{i\delta} & s_{23}c_{13}\\
s_{12}s_{23}-c_{12}c_{23}s_{13}e^{i\delta} &
-c_{12}s_{23}-s_{12}c_{23}s_{13}e^{i\delta} & c_{23}c_{13}
\end{array}\right)\left(\begin{array}{c}\nu_1\\ \nu_2\\ \nu_3 \end{array}\right)
$

And if that weren't crazy enough, the matrix above uses shorthand:
$c_{ij}=\cos(\theta_{ij})$ and $s_{ij}=\sin(\theta_{ij})$. If you set these
equations in motion and let time run for a bit, you find that an initial
electron neutrino, like one produced in the sun, propagates like this:

<figure>
  <img class="centerimage" src="../images/neutrino-oscillations.png" width="80%">
  <figcaption>
  Neutrino oscillations in action  
  Mathematica source code from en.wikipedia.org
  </figcaption>
</figure>

In the graphic above, an electron neutrino is produced, and its state
evolves based on the equations above. Black shows the probability the
neutrino is measured to be an electron neutrino after a given distance per
energy, while blue and red show the probability of measuring it as a muon
and tau neutrino, respectively.  

As you can see, there are periods of time during which it is far more
likely to detect this neutrino as a muon or tau neutrino than as an
electron-flavored one! This explains the deficit of electron neutrinos
observed from the sun, and relieved (astro)physicists of much distress.

Neutrinos of Astronomical Scale
---

Apart from the neutrinos from the Sun, we can also observe neutrinos from
high-energy cosmic events. The best example of this is supernova 1987A, a
stellar explosion in the Large Magellanic Cloud, a nearby galaxy, whose light
reached us in February of 1987.

As we've seen, in the early stages of a supernova, the iron core's electron
degeneracy pressure isn't enough to oppose gravitational collapse, and electron
capture ensues, in which the reaction $p+e^-\rightarrow n+ \nu_e$ turns the
core into an enormous atomic nucleus called a neutron star. This produces a
huge flux of neutrinos. And to make matters even more interesting, as these
neutrinos propagate through the incredibly dense core, they lose energy, much
of which is released in the form of neutrino-antineutrino pairs. (At least,
that's what I get from reading bits and pieces of the literature on the
subject.) The net result is an immense flux of neutrinos heading away from the
collapsing star. Outside the extremely dense core, the neutrinos can happily
propagate through just about everything, whereas light from the ensuing
explosion has to bounce around for quite a while before escaping. As a result,
the neutrinos leave the immediate vicinity of the star long before the actual
light, and carry well over 95% of the collapse's total energy.

Luckily for astrophysicists, there were several neutrino detectors in operation
on February 23, 1987. These detected an enormous flux of neutrinos for a short
period of time. Just because
I found this figure, I'm going to inflict it on you as well. It's the original
data from Kamiokande-II showing the huge number of events. They detected 11
events in a matter of minutes, as compared with just a couple each day under
normal circumstances. Another neutrino detector, IMB, near Lake Erie, detected
8 neutrinos at the same time.

<figure>
  <img class="centerimage" src="../images/neutrino-1987a-spike.png" width="80%">
  <figcaption>
  Kamiokande-II spots supernova 1987A. Nhit is the number
  of photomultipliers that recorded an event; any more than 20
  is considered a neutrino detection.  
  M. Koshiba et al., 1988
  </figcaption>
</figure>

In fact, the neutrino influx Kamiokande-II observed was so far above the
background that the experiment was able to use more of their detector than
usual for detection. The way that Kamiokande normally operates, as far as I
can tell, is that it only uses the central portion of the water chamber.
The outer layers are there just to filter out background, like cosmic rays
and radiation from detectors or other nearby objects. Only a small portion
of the detector can be used for the real detection of neutrinos because of
the incredibly low interaction rates. Any false positives would have a
major impact on the resulting data.

Furthermore, these neutrinos were detected around three hours before the light
from the explosion reached Earth, which confirms once again how awesome
neutrinos are as astrophysical tools. It also allowed physicists to place upper
limits on the mass of the neutrino, since more massive particles would have had
to travel much slower than light, and would likely have been overtaken by the
explosion's light on the way to Earth.

The Cosmic Neutrino Background
---

Once upon a time, almost 14 billion years ago, there was a Big Bang. In the
milliseconds afterwards, everything in the Universe (mostly electrons,
positrons, photons, and neutrinos) was in thermal equilibrium. What that
means is that the reaction $$\nu+\overline{\nu}\leftrightarrow e^+ + e^-$$
could occur in both directions. That means the energy of the neutrinos has to
be high enough to produce two more massive particles, and the cross section for
the interaction has to do with the temperature of the Universe. At a
temperature of around $10^{10}$ K, the temperature of the Universe dropped
below the critical temperature for the reaction shown above to proceed in both
directions, so neutrinos could fly free through the Universe without having to
worry about interacting with much else.

These neutrinos play a role in things like the ratio of protons to neutrons and
the frequency components of the cosmic microwave background, and we believe
that they are still around to this day. They're called the cosmic neutrino
background. While general relativity is needed to understand the time evolution
of the neutrino background, essentially what happens is that the neutrinos cool
as the Universe expands. Theorists predict the current energy of the neutrinos
in this background to be around $10^{-4}$ eV. To fully appreciate how cold
these guys are, let's take another look at our favorite neutrino-related graph,
the one showing flux vs. energy of neutrinos produced in hydrogen fusion.

<figure>
  <img class="centerimage" src="../images/solar-neutrino-spectrum.png" width="80%">
  <figcaption>
  Image from Bahcall, Solar Neutrinos.  
  http://www.sns.ias.edu/~jnb/Papers/Popular/Wiley/paper.pdf 
  </figcaption>
</figure>

Note that the neutrinos we were so excited to detect with gallium detectors
have energies around 0.3 mega electron volts. So the detection of the cosmic
neutrino background is, for the time being, an insurmountable experimental
challenge.

Even though we can't directly see the neutrino background, we have some good
reasons to believe that it exists. For one thing, the presence of neutrinos
affects the ratio of protons to neutrons, based on reactions like the
following: $$n\leftrightarrow p+e^-+\overline{\nu}_e\\ p+e^-\leftrightarrow n + \nu_e$$
As such, the presence of neutrinos throughout the Universe has dramatic effects
on nucleosynthesis, the process by which nucleons combine into larger nuclei,
and the current abundances give indirect evidence for the cosmic neutrino
background.

For another thing, apparently the angular frequency components of the cosmic
microwave background's temperature fluctuations are "damped" relative to
predictions in the absence of the neutrino background. I believe that has to do
with the way in which vibrations propagate through the very early Universe.
Neutrinos provided a certain amount of diffusion, which served to make the
Universe more isotropic than it would have been in their absence.

Why do we care about this? We know very little about the early stages of the
Universe's evolution. Some of our most useful information comes from
observations of the cosmic microwave background, which decoupled from the rest
of matter when the Universe was around 300 years old. The neutrino background,
if detected, can tell us quite a bit about the intervening time period, which
gets us that much closer to understanding the highest-energy event in the
history of the Universe.

Outstanding mysteries
---

As you've probably guessed by now, there's a lot we don't know about neutrinos
and how they function in astrophysics. They have a lot of mysteries in store
for us. Here are a few:

* Value of $\theta_{13}$ and the neutrino mass hierarchy. Remember the mixing
angles from the discussion of neutrino oscillations? Well, we know two of them
fairly well, including their signs, but we have very little information on
$\theta_{13}$. This is particularly interesting because the sign of that
particular mixing angle will tell us how the masses of the mass eigenstates are
related. There are two possibilities: the natural mass hierarchy, in which
$m_{\nu_1}<m_{\nu_2}<m_{\nu_3}$ (shown at left below), and the inverted mass
hierarchy, in which $m_{\nu_3}<m_{\nu_1}<m_{\nu_2}$ (shown at right below).
<figure>
  <img class="centerimage" src="../images/neutrinomasshierarchy.jpg" width="80%">
  <figcaption>
    From Nishikawa, K. 2010 _Recent Status of Accelerator Neutrino Experiments_
  </figcaption>
</figure>


* Dirac/Majorana neutrinos. There is a bit of an outstanding question on
whether the neutrino has a distinct antiparticle (Dirac neutrino) or is its own
antiparticle (Majorana neutrino). If it is a Majorana particle, then processes
such as neutrinoless double-beta decay could occur, in which two neutrinos
annihilate in joint beta decay of two neutrons. This is a highly sought-after
reaction, and there are legions of physicists working to find or rule out such
a reaction.
<figure>
  <img class="centerimage" src="../images/doublebeta.png" width="80%">
  <figcaption>
    Double-beta neutron decay.  
    From wikipedia.org.
  </figcaption>
</figure>

* Other neutrino sources. We've spotted neutrinos from a supernova before,
but we should also be able to detect some from other astrophysical sources,
like active galactic nuclei (really bright spots caused by accretion around a
supermassive black hole) and gamma ray bursts (possibly bright bursts from
rapidly rotating supernovae - is physics awesome, or what?). Detecting
neutrinos from such events may help us understand what's going on in
high-energy events.

In summary, neutrinos are pretty awesome little particles. They can be useful
in understanding phenomena that light can't easily bring us information about,
and they have a lot of mysteries left for us to sort out.

PeV-energy neutrinos
---

Well, I thought I was done with neutrino astrophysics when I wrapped up the
previous section, but then I read about the recent discovery of two extremely
high-energy neutrinos by the IceCube collaboration. As I've mentioned before,
IceCube is a Cerenkov detector. Its 'detector' consists of about a cubic
kilometer of ice in the Antarctic, and it has 86 strings of detectors suspended
in the ice. They typically detect solar and atmospheric neutrinos, but in an
article submitted to Phys. Rev. Letters last month, they report the detection
of two peta-electron volt neutrinos. That's $10^{15}$ electron volts, or
around the kinetic energy of a few hundred mosquitos! While that might not
sound like much, packing all that energy into a nearly massless particle is
quite impressive. In a slightly more reasonable comparison, these neutrinos
are much more energetic than protons in the Large Hadron Collider over in
CERN, which are accelerated to a measly 3.5 TeV. That's
_tera_electronvolts, as compared with the _peta_electronvolt-energy
neutrinos, a difference of 3 orders of magnitude.

These high-energy events are very unlikely to have resulted from cosmic ray
interactions with the atmosphere (though the collaboration is investigating
the possibility that they resulted from the decay of charmed particles
produced by high-energy cosmics), and the collaboration is fairly sure that
they're actually a product of some extremely high-energy astrophysical
events. In the paper, they suggest gamma ray bursts or active galactic
nuclei as potential sources, and they're hopeful that further analysis will
reveal more similarly energetic neutrinos or give clues to their origin.

Oh, and just as proof that physicists have a sense of humor, the detected
neutrinos have been named Bert and Ernie. It's even in the graphics for the
paper!

<figure>
  <img class="centerimage" src="../images/neutrino-bert-ernie.png" width="80%">
  <figcaption>
  From _First observation of PeV-energy neutrinos with IceCube_, IceCube
collaboration, 2013
  </figcaption>
</figure>


The above graphic (taken from the paper) provides a visualization of the events
in question. Each sphere shows the data from one photomultiplier tube embedded
in the ice. The size of the sphere shows how many photoelectrons were detected,
and the color shows the time at which the detection occurred, from red for the
first detections to blue for the last. The energy of the event can be
determined from the total number of photoelectrons detected, and the
collaboration calculated energies of 1.04 and 1.14 PeV for Bert and Ernie,
respectively.

For those interested, the paper in question can be found [here](http://arxiv.org/pdf/1304.5356v1.pdf).
