FROM amd64/ubuntu as base

RUN apt update && \
    apt install -y curl && \
    apt install -y tar && \
    apt install -y xz-utils

RUN addgroup --system nixbld && \
    adduser --home /home/nix --disabled-password --gecos "" --shell /bin/bash nix && \
    adduser nix nixbld && \
    mkdir -m 0755 /nix && chown nix /nix && \
    mkdir -p /etc/nix && echo 'sandbox = false' > /etc/nix/nix.conf

CMD /bin/bash -l
USER nix
ENV USER nix
WORKDIR /home/nix

COPY --chown=nix:nix ./nix/install.sh .

RUN touch .bash_profile && /home/nix/install.sh 

ENV PATH="/home/nix/bin:${PATH}"

FROM base as front-build

ARG api_login
ARG api_pass

ENV API_LOGIN_ENV ${api_login}
ENV API_PASS_ENV ${api_pass}

WORKDIR /build

COPY --chown=nix:nix . .

RUN . /home/nix/.nix-profile/etc/profile.d/nix.sh && \ 
      nix-env -i purescript && \
      nix-shell ./nix/build.nix \ 
      --log-format bar-with-logs --command \
      "npm install && npm run api && npm run bundle"

FROM base as main

EXPOSE 3000/tcp

WORKDIR /front

COPY --from=front-build --chown=nix:nix /build/app /front/app
COPY --from=front-build --chown=nix:nix /build/node_modules /front/node_modules
COPY --from=front-build --chown=nix:nix /build/output /front/output
COPY --from=front-build --chown=nix:nix /build/nix/prod.nix /build/deploy /build/index.js /build/config.json /build/*.mjs /build/package.json /front/

ENTRYPOINT ["/front/init.sh"]