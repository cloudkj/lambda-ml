(ns lambda-ml.q-network
  (:require [lambda-ml.core :as c]
            [lambda-ml.neural-network :as nn]))

;; Environment

(defn environment-reset
  [env]
  ((:reset env) (:actual env)))

(defn environment-step
  [env action]
  ((:step env) (:actual env) action))

(defn environment-step-reward
  [env step]
  ((:reward env) step))

(defn environment-step-state
  [env step]
  ((:state env) step))

(defn environment-step-done
  [env step]
  ((:done env) step))

(defn make-environment
  [env reset step reward state done]
  {:actual env
   :reset reset
   :step step
   :reward reward
   :state state
   :done done})

;; Q Network

(defn record
  [model env state action step]
  (let [{memory :memory capacity :capacity} model
        observation {:from state
                     :action action
                     :to     (environment-step-state env step)
                     :reward (environment-step-reward env step)
                     :done   (environment-step-done env step)}]
    ;; TODO: can define a bounded queue to be used as memory data structure
    (->> (if (< (count memory) capacity)
           (conj memory observation)
           (conj (subvec memory 1) observation))
         (assoc model :memory))))

(defn replay
  ([model]
   (let [{memory :memory batch :batch} model]
     (if (< (count memory) batch)
       model
       (replay model (c/sample-without-replacement memory batch)))))
  ([model observations]
   (let [{discount :discount network :network} model
         ps (nn/neural-network-predict network (map :from observations))
         qs (nn/neural-network-predict network (map :to observations))
         targets (-> (fn [p q obs]
                       (let [{a :action r :reward done :done} obs
                             reward (if done
                                      r
                                      (+ r (* discount (apply max q))))]
                         (assoc p a reward)))
                     (map ps qs observations))]
     (->> (nn/neural-network-fit network (map :from observations) targets)
          (assoc model :network)))))

(defn act-explore
  "Explore: select random action"
  [model]
  (let [outputs (last (get-in model [:network :layers]))]
    ;; TODO: model should store information about # of output, and a function can be used to sample random action from action space
    (rand-int outputs)))

(defn act-exploit
  "Exploit: select best action with maximum Q-value"
  [model state]
  (let [{network :network} model]
    (->> (nn/neural-network-predict network [state])
         (first)
         (map-indexed vector)
         (apply max-key second)
         (first))))

(defn act
  [model state]
  (let [{explore :explore} model]
    (if (< (rand) (- 1.0 explore))
      (act-exploit model state)
      (act-explore model))))

(defn simulate
  ([model env]
   (simulate model env (environment-reset env) 0))
  ([model env state reward]
   (let [{stop? :stop memory :memory batch :batch} model
         {:keys [explore-decay explore-min]} model
         action (act model state)
         step (environment-step env action)
         model (record model env state action step)
         model (replay model)
         model (cond-> model
                 (>= (count memory) batch)
                 (update :explore #(max (* % explore-decay) explore-min)))]
     (if (or (environment-step-done env step) (stop? reward))
       [model reward]
       (recur model
              env
              (environment-step-state env step)
              (+ reward (environment-step-reward env step)))))))

(defn q-network-fit
  [model env]
  (lazy-seq (let [[m r] (simulate model env)]
              (cons [m r] (q-network-fit m env)))))

(defn q-network-predict
  [model env state]
  (act-exploit model state))

(defn evaluate
  [model env]
  (loop [state (environment-reset env)
         reward 0]
    (let [action (q-network-predict model env state)
          step (environment-step env action)
          r (environment-step-reward env step)]
      (cond (environment-step-done env step) reward
            ((:stop model) reward)           reward
            :else
            (recur (environment-step-state env step) (+ r reward))))))

(defn make-q-network
  [network capacity batch discount explore-decay explore-min stop]
  {:network network
   :memory []
   :capacity capacity
   :batch batch
   :discount discount
   :explore 1.0
   :explore-decay explore-decay
   :explore-min explore-min
   :stop stop})
